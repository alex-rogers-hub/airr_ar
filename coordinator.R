# /home/aarogers/airr/coordinator.R
# Runs via cron. Groups accounts, submits AWS Batch jobs, exits.

library(DBI)
library(RPostgres)
library(aws.batch)   # or httr2 calls to AWS API
library(jsonlite)

source("global_scripts/app_helper_functions.R")

ACCOUNTS_PER_GROUP <- 20   # tune this — more = fewer containers, 
# less = faster but more overhead

coordinator_main <- function() {
  
  log_info("Coordinator starting")
  
  con <- make_con()
  on.exit(dbDisconnect(con))
  
  # 1. Get all active login_ids that need scoring today
  active_accounts <- get_active_accounts_for_scoring(con)
  log_info(sprintf("Found %d accounts to score", length(active_accounts)))
  
  if (length(active_accounts) == 0) {
    log_info("No accounts to score. Exiting.")
    return(invisible(NULL))
  }
  
  # 2. Group accounts
  groups <- split(
    active_accounts,
    ceiling(seq_along(active_accounts) / ACCOUNTS_PER_GROUP)
  )
  log_info(sprintf("Split into %d groups of up to %d accounts", 
                   length(groups), ACCOUNTS_PER_GROUP))
  
  # 3. Create a run_id for this scoring run (for monitoring)
  run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # 4. Write groups to job queue table so we can track them
  job_ids <- submit_batch_groups(con, groups, run_id)
  
  # 5. Submit to AWS Batch
  batch_job_ids <- lapply(seq_along(groups), function(i) {
    submit_aws_batch_job(
      group_index    = i,
      login_ids      = groups[[i]],
      run_id         = run_id,
      db_job_id      = job_ids[[i]]
    )
  })
  
  log_info(sprintf("Submitted %d AWS Batch jobs. Run ID: %s", 
                   length(batch_job_ids), run_id))
  log_info("Coordinator done — workers will handle scoring in parallel")
}

get_active_accounts_for_scoring <- function(con) {
  # Accounts that are active (have valid subscription or are demo)
  # and have at least one brand tracked
  query <- "
    SELECT DISTINCT u.login_id
    FROM dim_user u
    JOIN fact_user_brands_tracked fubt 
      ON u.login_id = fubt.login_id
    LEFT JOIN fact_user_sub_level fusl 
      ON u.login_id = fusl.login_id
      AND CURRENT_DATE BETWEEN fusl.date_valid_from 
                           AND COALESCE(fusl.date_valid_to, '9999-12-31')
    WHERE fubt.date_valid_to IS NULL
      AND u.is_demo = FALSE
      AND (
        fusl.subscription_level_id IS NOT NULL  -- has active subscription
        OR u.is_demo = TRUE
      )
    ORDER BY u.login_id
  "
  result <- dbGetQuery(con, query)
  result$login_id
}

submit_batch_groups <- function(con, groups, run_id) {
  # Write to a new monitoring table (see DB changes section)
  lapply(seq_along(groups), function(i) {
    login_ids_json <- toJSON(groups[[i]])
    dbExecute(con, "
      INSERT INTO dim_scoring_run_groups 
        (run_id, group_index, login_ids, status, created_at)
      VALUES ($1, $2, $3, 'pending', NOW())
      RETURNING group_id
    ", list(run_id, i, login_ids_json))
    
    result <- dbGetQuery(con, "
      SELECT group_id FROM dim_scoring_run_groups 
      WHERE run_id = $1 AND group_index = $2
    ", list(run_id, i))
    result$group_id
  })
}