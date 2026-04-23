# worker_entry.R
# Entry point for the Docker container.
# AWS Batch passes the job parameters as environment variables.

library(jsonlite)
library(DBI)
library(RPostgres)

# Simple logging helper — replaces logger package
log_info  <- function(...) message(paste0("[INFO] ", paste(...), 
                                          " [", format(Sys.time(), "%H:%M:%S"), "]"))
log_error <- function(...) message(paste0("[ERROR] ", paste(...), 
                                          " [", format(Sys.time(), "%H:%M:%S"), "]"))

log_info("Worker starting")

# ── 1. Read job parameters from environment ──────────────────────
login_ids_json <- Sys.getenv("LOGIN_IDS", unset = NA)
run_id         <- Sys.getenv("RUN_ID",    unset = NA)
group_id       <- Sys.getenv("GROUP_ID",  unset = NA)
model          <- Sys.getenv("OPENAI_MODEL", unset = "gpt-4.1-mini")

if (is.na(login_ids_json)) {
  log_error("LOGIN_IDS environment variable not set. Exiting.")
  quit(status = 1)
}

login_ids <- fromJSON(login_ids_json)

log_info(paste("Run ID:", run_id))
log_info(paste("Group ID:", group_id))
log_info(paste("Accounts:", paste(login_ids, collapse = ", ")))

# ── 2. Set up SSH tunnel to database ────────────────────────────
log_info("Setting up SSH tunnel...")
tunnel_result <- system("bash /app/setup_tunnel.sh", wait = TRUE)
if (tunnel_result != 0) {
  log_error("SSH tunnel setup failed")
  quit(status = 1)
}
log_info("SSH tunnel ready")

# ── 3. Source existing scoring functions ─────────────────────────
log_info("Sourcing scoring functions...")
source("/app/global_scripts/app_helper_functions.R")
source("/app/global_scripts/chatgpt_functions.R")
source("/app/global_scripts/queries_send_return.R")
source("/app/global_scripts/upload_functions.R")
source("/app/global_scripts/presence.R")
source("/app/global_scripts/perception.R")
source("/app/global_scripts/prestige.R")
source("/app/global_scripts/persistence.R")
source("/app/global_scripts/full_airr_score.R")
source("/app/global_scripts/profile_scoring.R")
source("/app/global_scripts/worker_orchestrator.R")
log_info("All functions sourced OK")

# ── 4. Connect to database ───────────────────────────────────────
log_info("Connecting to database...")
con <- tryCatch(
  make_con(),
  error = function(e) {
    log_error(paste("Database connection failed:", e$message))
    quit(status = 1)
  }
)
on.exit({
  try(dbDisconnect(con), silent = TRUE)
  log_info("Database connection closed")
}, add = TRUE)
log_info("Database connected OK")

# ── 5. Update status to 'running' ───────────────────────────────
if (!is.na(group_id) && group_id != "0") {
  tryCatch(
    dbExecute(con, "
      UPDATE dim_scoring_run_groups 
      SET status = 'running', started_at = NOW()
      WHERE group_id = $1
    ", list(as.integer(group_id))),
    error = function(e) log_error(paste("Status update failed:", e$message))
  )
}

# ── 6. Run scoring ───────────────────────────────────────────────
log_info("Starting worker_score_accounts...")
result <- tryCatch({
  worker_score_accounts(
    con       = con,
    login_ids = login_ids,
    model     = model,
    run_id    = run_id
  )
}, error = function(e) {
  log_error(paste("Fatal error in worker_score_accounts:", e$message))
  NULL
})

# ── 7. Update status to 'complete' or 'failed' ──────────────────
final_status <- if (!is.null(result)) "complete" else "failed"

if (!is.na(group_id) && group_id != "0") {
  tryCatch(
    dbExecute(con, "
      UPDATE dim_scoring_run_groups 
      SET status = $1, completed_at = NOW()
      WHERE group_id = $2
    ", list(final_status, as.integer(group_id))),
    error = function(e) log_error(paste("Status update failed:", e$message))
  )
}

log_info(paste("Worker finished with status:", final_status))
quit(status = if (!is.null(result)) 0 else 1)