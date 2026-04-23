# ============================================
# admin_scripts/admin_account_management.R
#
# Run these functions from RStudio on the
# web or compute server with an active pool
# connection.
#
# Usage:
#   source("admin_scripts/admin_account_management.R")
#   set_extra_competitors("user@example.com", 15)
# ============================================

# ============================================
# Helper — find a user by email
# ============================================

.get_user <- function(email) {
  result <- dbGetQuery(pool,
                       "SELECT login_id, email, account_name,
            is_linked_account, primary_login_id
     FROM dim_user
     WHERE email = $1",
                       params = list(tolower(trimws(email))))
  
  if (nrow(result) == 0) {
    cat(sprintf("✗ User not found: %s\n", email))
    return(NULL)
  }
  result
}

.get_subscription <- function(login_id) {
  dbGetQuery(pool, "
    SELECT fus.*, ds.subscription_name,
           ds.num_competitors_included,
           ds.num_prompts_included,
           ds.num_personas_included
    FROM fact_user_sub_level fus
    JOIN dim_subscription ds
      ON ds.subscription_level_id = fus.subscription_level_id
    WHERE fus.login_id = $1
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    ORDER BY ds.num_competitors_included DESC
    LIMIT 1",
             params = list(login_id))
}

.print_current_slots <- function(login_id) {
  sub <- .get_subscription(login_id)
  if (nrow(sub) == 0) {
    cat("  No active subscription found\n")
    return(invisible(NULL))
  }
  cat(sprintf("  Plan:        %s\n", sub$subscription_name))
  cat(sprintf("  Competitors: %d base + %d extra = %d total\n",
              sub$num_competitors_included,
              sub$extra_competitors_added %||% 0,
              sub$num_competitors_included + (sub$extra_competitors_added %||% 0)))
  cat(sprintf("  Prompts:     %d base + %d extra = %d total\n",
              sub$num_prompts_included,
              sub$extra_prompts_added %||% 0,
              sub$num_prompts_included + (sub$extra_prompts_added %||% 0)))
  cat(sprintf("  Personas:    %d base + %d extra = %d total\n",
              sub$num_personas_included %||% 0,
              sub$extra_personas_added %||% 0,
              (sub$num_personas_included %||% 0) + (sub$extra_personas_added %||% 0)))
}

# ============================================
# Set extra competitors for a user
#
# total_competitors = the total number you want
# the user to have (base + extra combined)
# e.g. if plan includes 10 and you want 15,
# pass total_competitors = 15
# ============================================

set_extra_competitors <- function(email, total_competitors) {
  
  user <- .get_user(email)
  if (is.null(user)) return(invisible(NULL))
  login_id <- user$login_id[1]
  
  cat(sprintf("\n=== set_extra_competitors: %s ===\n", email))
  cat("Current:\n")
  .print_current_slots(login_id)
  
  sub <- .get_subscription(login_id)
  if (nrow(sub) == 0) {
    cat("✗ No active subscription — cannot update\n")
    return(invisible(NULL))
  }
  
  base_competitors <- sub$num_competitors_included[1]
  extra            <- max(0, as.integer(total_competitors) - base_competitors)
  
  dbExecute(pool, "
    UPDATE fact_user_sub_level
    SET extra_competitors_added = $1
    WHERE login_id = $2
      AND date_valid_from <= CURRENT_DATE
      AND date_valid_to >= CURRENT_DATE",
            params = list(extra, login_id))
  
  cat("\nUpdated:\n")
  .print_current_slots(login_id)
  cat(sprintf("\n✓ Competitors set to %d (%d base + %d extra)\n",
              total_competitors, base_competitors, extra))
  
  return(invisible(list(
    login_id          = login_id,
    email             = email,
    total_competitors = total_competitors,
    base              = base_competitors,
    extra             = extra
  )))
}

# ============================================
# Set extra prompts for a user
# ============================================

set_extra_prompts <- function(email, total_prompts) {
  
  user <- .get_user(email)
  if (is.null(user)) return(invisible(NULL))
  login_id <- user$login_id[1]
  
  cat(sprintf("\n=== set_extra_prompts: %s ===\n", email))
  cat("Current:\n")
  .print_current_slots(login_id)
  
  sub <- .get_subscription(login_id)
  if (nrow(sub) == 0) {
    cat("✗ No active subscription — cannot update\n")
    return(invisible(NULL))
  }
  
  base_prompts <- sub$num_prompts_included[1]
  extra        <- max(0, as.integer(total_prompts) - base_prompts)
  
  dbExecute(pool, "
    UPDATE fact_user_sub_level
    SET extra_prompts_added = $1
    WHERE login_id = $2
      AND date_valid_from <= CURRENT_DATE
      AND date_valid_to >= CURRENT_DATE",
            params = list(extra, login_id))
  
  cat("\nUpdated:\n")
  .print_current_slots(login_id)
  cat(sprintf("\n✓ Prompts set to %d (%d base + %d extra)\n",
              total_prompts, base_prompts, extra))
  
  return(invisible(list(
    login_id     = login_id,
    email        = email,
    total_prompts = total_prompts,
    base         = base_prompts,
    extra        = extra
  )))
}

# ============================================
# Set extra personas for a user
# ============================================

set_extra_personas <- function(email, total_personas) {
  
  user <- .get_user(email)
  if (is.null(user)) return(invisible(NULL))
  login_id <- user$login_id[1]
  
  cat(sprintf("\n=== set_extra_personas: %s ===\n", email))
  cat("Current:\n")
  .print_current_slots(login_id)
  
  sub <- .get_subscription(login_id)
  if (nrow(sub) == 0) {
    cat("✗ No active subscription — cannot update\n")
    return(invisible(NULL))
  }
  
  base_personas <- sub$num_personas_included[1] %||% 0
  extra         <- max(0, as.integer(total_personas) - base_personas)
  
  # Add column if it doesn't exist yet
  tryCatch(
    dbExecute(pool, "
      ALTER TABLE fact_user_sub_level
      ADD COLUMN IF NOT EXISTS extra_personas_added INTEGER DEFAULT 0"),
    error = function(e) invisible(NULL)
  )
  
  dbExecute(pool, "
    UPDATE fact_user_sub_level
    SET extra_personas_added = $1
    WHERE login_id = $2
      AND date_valid_from <= CURRENT_DATE
      AND date_valid_to >= CURRENT_DATE",
            params = list(extra, login_id))
  
  cat("\nUpdated:\n")
  .print_current_slots(login_id)
  cat(sprintf("\n✓ Personas set to %d (%d base + %d extra)\n",
              total_personas, base_personas, extra))
  
  return(invisible(list(
    login_id       = login_id,
    email          = email,
    total_personas = total_personas,
    base           = base_personas,
    extra          = extra
  )))
}

# ============================================
# Update all three at once
# ============================================

set_all_slots <- function(email,
                          total_competitors = NULL,
                          total_prompts     = NULL,
                          total_personas    = NULL) {
  
  user <- .get_user(email)
  if (is.null(user)) return(invisible(NULL))
  login_id <- user$login_id[1]
  
  cat(sprintf("\n=== set_all_slots: %s ===\n", email))
  cat("Before:\n")
  .print_current_slots(login_id)
  
  sub <- .get_subscription(login_id)
  if (nrow(sub) == 0) {
    cat("✗ No active subscription\n")
    return(invisible(NULL))
  }
  
  updates <- c()
  params  <- list()
  idx     <- 1
  
  if (!is.null(total_competitors)) {
    extra_comp <- max(0, total_competitors - sub$num_competitors_included[1])
    updates    <- c(updates, sprintf("extra_competitors_added = $%d", idx))
    params     <- c(params, list(extra_comp))
    idx        <- idx + 1
  }
  
  if (!is.null(total_prompts)) {
    extra_prom <- max(0, total_prompts - sub$num_prompts_included[1])
    updates    <- c(updates, sprintf("extra_prompts_added = $%d", idx))
    params     <- c(params, list(extra_prom))
    idx        <- idx + 1
  }
  
  if (!is.null(total_personas)) {
    base_p     <- sub$num_personas_included[1] %||% 0
    extra_pers <- max(0, total_personas - base_p)
    updates    <- c(updates, sprintf("extra_personas_added = $%d", idx))
    params     <- c(params, list(extra_pers))
    idx        <- idx + 1
  }
  
  if (length(updates) == 0) {
    cat("Nothing to update — pass at least one of total_competitors, total_prompts, total_personas\n")
    return(invisible(NULL))
  }
  
  params <- c(params, list(login_id))
  
  dbExecute(pool,
            sprintf("UPDATE fact_user_sub_level SET %s
             WHERE login_id = $%d
               AND date_valid_from <= CURRENT_DATE
               AND date_valid_to >= CURRENT_DATE",
                    paste(updates, collapse = ", "), idx),
            params = params)
  
  cat("\nAfter:\n")
  .print_current_slots(login_id)
  cat("\n✓ Slots updated\n")
  
  return(invisible(NULL))
}

# ============================================
# Rename a linked account
# ============================================

rename_linked_account <- function(email, new_name) {
  
  user <- .get_user(email)
  if (is.null(user)) return(invisible(NULL))
  
  if (!isTRUE(user$is_linked_account[1])) {
    cat(sprintf(
      "! Warning: %s is not flagged as a linked account.\n", email))
    cat("  Updating account_name anyway — press Ctrl+C to cancel.\n")
    Sys.sleep(3)
  }
  
  login_id     <- user$login_id[1]
  old_name     <- user$account_name[1] %||% "(none)"
  new_name_str <- trimws(new_name)
  
  if (!nzchar(new_name_str)) {
    cat("✗ New name cannot be blank\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("\n=== rename_linked_account ===\n"))
  cat(sprintf("  Email:    %s\n", email))
  cat(sprintf("  Old name: %s\n", old_name))
  cat(sprintf("  New name: %s\n", new_name_str))
  
  dbExecute(pool,
            "UPDATE dim_user SET account_name = $1 WHERE login_id = $2",
            params = list(new_name_str, login_id))
  
  # Verify
  updated <- dbGetQuery(pool,
                        "SELECT login_id, email, account_name FROM dim_user WHERE login_id = $1",
                        params = list(login_id))
  
  cat(sprintf("\n✓ Account renamed to: \"%s\"\n",
              updated$account_name[1]))
  
  return(invisible(list(
    login_id = login_id,
    email    = email,
    old_name = old_name,
    new_name = new_name_str
  )))
}

# ============================================
# View all users and their current slots
# ============================================

view_all_user_slots <- function() {
  
  result <- dbGetQuery(pool, "
    SELECT
      u.login_id,
      u.email,
      COALESCE(u.account_name, b.brand_name, u.email) AS account_name,
      u.is_linked_account,
      ds.subscription_name,
      ds.num_competitors_included + COALESCE(fus.extra_competitors_added, 0)
        AS total_competitors,
      ds.num_prompts_included + COALESCE(fus.extra_prompts_added, 0)
        AS total_prompts,
      COALESCE(ds.num_personas_included, 0) +
        COALESCE(fus.extra_personas_added, 0)
        AS total_personas,
      COALESCE(fus.extra_competitors_added, 0) AS extra_competitors,
      COALESCE(fus.extra_prompts_added, 0)     AS extra_prompts,
      COALESCE(fus.extra_personas_added, 0)    AS extra_personas
    FROM dim_user u
    LEFT JOIN fact_user_sub_level fus
      ON fus.login_id = u.login_id
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    LEFT JOIN dim_subscription ds
      ON ds.subscription_level_id = fus.subscription_level_id
    LEFT JOIN fact_user_brands_tracked ubt
      ON ubt.login_id = u.login_id
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LEFT JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE u.onboarding_complete = TRUE
      AND (u.is_demo = FALSE OR u.is_demo IS NULL)
    ORDER BY u.login_id
  ")
  
  print(result)
  return(invisible(result))
}

# ============================================
# Rescore a single user account
# Queues scoring jobs for all their brands,
# prompts and personas
# ============================================

rescore_user <- function(email,
                         include_brands  = TRUE,
                         include_prompts = TRUE,
                         include_personas = TRUE,
                         model           = "gpt-4.1-mini") {
  
  user <- .get_user(email)
  if (is.null(user)) return(invisible(NULL))
  login_id <- user$login_id[1]
  
  cat(sprintf("\n=== rescore_user: %s (login_id: %d) ===\n",
              email, login_id))
  
  # ── Get active brands ──────────────────────────────────────────────
  brands <- dbGetQuery(pool, "
    SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    ORDER BY ubt.main_brand_flag DESC, b.brand_name",
                       params = list(login_id))
  
  cat(sprintf("  Brands:   %d (%d main + %d competitors)\n",
              nrow(brands),
              sum(brands$main_brand_flag),
              sum(!brands$main_brand_flag)))
  
  # ── Get active prompts ─────────────────────────────────────────────
  queries <- get_user_tracked_queries(login_id)
  cat(sprintf("  Prompts:  %d\n", nrow(queries)))
  
  # ── Get active personas ────────────────────────────────────────────
  personas <- dbGetQuery(pool, "
    SELECT profile_id FROM fact_user_profiles_tracked
    WHERE login_id = $1
      AND date_valid_from <= CURRENT_DATE
      AND (date_valid_to IS NULL OR date_valid_to > CURRENT_DATE)",
                         params = list(login_id))
  cat(sprintf("  Personas: %d\n", nrow(personas)))
  
  jobs_queued <- 0
  
  # ── Queue brand scoring jobs ───────────────────────────────────────
  if (include_brands && nrow(brands) > 0) {
    
    prompt_map <- if (nrow(queries) > 0) {
      setNames(as.list(queries$query_id), queries$query_string)
    } else list()
    
    for (i in seq_len(nrow(brands))) {
      bid       <- brands$brand_id[i]
      bname     <- brands$brand_name[i]
      is_main   <- isTRUE(brands$main_brand_flag[i])
      job_type  <- if (is_main) "score_brand" else "score_competitor"
      
      dbExecute(pool, "
        INSERT INTO dim_job_queue (job_type, login_id, payload)
        VALUES ($1, $2, $3)",
                params = list(
                  job_type,
                  login_id,
                  jsonlite::toJSON(list(
                    brand_name = bname,
                    brand_id   = bid,
                    prompts    = prompt_map
                  ), auto_unbox = TRUE)
                ))
      
      cat(sprintf("  \u2713 Queued %s: %s\n", job_type, bname))
      jobs_queued <- jobs_queued + 1
    }
  }
  
  # ── Queue prompt scoring jobs ──────────────────────────────────────
  if (include_prompts && nrow(queries) > 0) {
    
    brand_ids <- brands$brand_id
    
    for (i in seq_len(nrow(queries))) {
      dbExecute(pool, "
        INSERT INTO dim_job_queue (job_type, login_id, payload)
        VALUES ('score_query', $1, $2)",
                params = list(
                  login_id,
                  jsonlite::toJSON(list(
                    query_string = queries$query_string[i],
                    query_id     = queries$query_id[i],
                    brand_ids    = brand_ids
                  ), auto_unbox = TRUE)
                ))
      
      cat(sprintf("  \u2713 Queued prompt: %s\n",
                  substr(queries$query_string[i], 1, 60)))
      jobs_queued <- jobs_queued + 1
    }
  }
  
  # ── Queue persona scoring jobs ─────────────────────────────────────
  if (include_personas && nrow(personas) > 0) {
    
    for (i in seq_len(nrow(personas))) {
      dbExecute(pool, "
        INSERT INTO dim_job_queue (job_type, login_id, payload)
        VALUES ('score_profile', $1, $2)",
                params = list(
                  login_id,
                  jsonlite::toJSON(list(
                    profile_id = personas$profile_id[i]
                  ), auto_unbox = TRUE)
                ))
      
      cat(sprintf("  \u2713 Queued persona: %d\n", personas$profile_id[i]))
      jobs_queued <- jobs_queued + 1
    }
  }
  
  cat(sprintf("\n\u2713 %d job%s queued for %s\n",
              jobs_queued,
              if (jobs_queued != 1) "s" else "",
              email))
  cat("  Jobs will be picked up by the worker process.\n")
  cat("  Monitor progress:\n")
  cat(sprintf("  dbGetQuery(pool, \"SELECT job_type, status, COUNT(*) as n FROM dim_job_queue WHERE login_id = %d GROUP BY job_type, status\")\n",
              login_id))
  
  return(invisible(list(
    login_id    = login_id,
    email       = email,
    jobs_queued = jobs_queued
  )))
}

# ============================================
# Run scoring directly (bypasses job queue)
# Use this if you need it done immediately
# rather than waiting for the worker
# ============================================

rescore_user_direct <- function(email,
                                include_brands   = TRUE,
                                include_prompts  = TRUE,
                                include_personas = TRUE,
                                model            = "gpt-4.1-mini") {
  
  user <- .get_user(email)
  if (is.null(user)) return(invisible(NULL))
  login_id <- user$login_id[1]
  
  cat(sprintf("\n=== rescore_user_direct: %s (login_id: %d) ===\n",
              email, login_id))
  cat("  Note: this runs synchronously and may take several minutes.\n\n")
  
  con <- make_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  brands <- dbGetQuery(con, "
    SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    ORDER BY ubt.main_brand_flag DESC, b.brand_name",
                       params = list(login_id))
  
  queries <- dbGetQuery(con, "
    SELECT dq.query_id, dq.query_string
    FROM fact_user_queries_tracked uqt
    JOIN dim_query dq ON dq.query_id = uqt.query_id
    WHERE uqt.login_id = $1
      AND uqt.date_valid_from <= CURRENT_DATE
      AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)",
                        params = list(login_id))
  
  personas <- dbGetQuery(con, "
    SELECT upt.profile_id, dcp.profile_name
    FROM fact_user_profiles_tracked upt
    JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
    WHERE upt.login_id = $1
      AND upt.date_valid_from <= CURRENT_DATE
      AND (upt.date_valid_to IS NULL OR upt.date_valid_to > CURRENT_DATE)",
                         params = list(login_id))
  
  failed <- c()
  
  # ── Score brands ───────────────────────────────────────────────────
  if (include_brands && nrow(brands) > 0) {
    cat(sprintf("Scoring %d brand(s)...\n", nrow(brands)))
    
    for (i in seq_len(nrow(brands))) {
      bname <- brands$brand_name[i]
      tryCatch({
        t0 <- Sys.time()
        cat(sprintf("  [%d/%d] %s...", i, nrow(brands), bname))
        user_create_airr(con, bname, login_id, model)
        cat(sprintf(" \u2713 (%.0fs)\n",
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      }, error = function(e) {
        cat(sprintf(" \u2717 %s\n", e$message))
        failed <<- c(failed, paste0("brand:", bname))
      })
    }
  }
  
  # ── Score prompts ──────────────────────────────────────────────────
  if (include_prompts && nrow(queries) > 0) {
    cat(sprintf("\nScoring %d prompt(s) across %d brand(s)...\n",
                nrow(queries), nrow(brands)))
    
    brand_ids <- brands$brand_id
    
    for (q in seq_len(nrow(queries))) {
      qid     <- queries$query_id[q]
      qstring <- queries$query_string[q]
      
      tryCatch({
        t0 <- Sys.time()
        cat(sprintf("  [%d/%d] %s...",
                    q, nrow(queries), substr(qstring, 1, 50)))
        create_prompt_airr_multiple(con, brand_ids, qstring,
                                    qid, login_id, model)
        cat(sprintf(" \u2713 (%.0fs)\n",
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      }, error = function(e) {
        cat(sprintf(" \u2717 %s\n", e$message))
        failed <<- c(failed, paste0("prompt:", qid))
      })
    }
  }
  
  # ── Score personas ─────────────────────────────────────────────────
  if (include_personas && nrow(personas) > 0) {
    cat(sprintf("\nScoring %d persona(s)...\n", nrow(personas)))
    
    for (p in seq_len(nrow(personas))) {
      pid   <- personas$profile_id[p]
      pname <- personas$profile_name[p]
      
      tryCatch({
        t0 <- Sys.time()
        cat(sprintf("  [%d/%d] %s...", p, nrow(personas), pname))
        score_profile(con, login_id, pid, model)
        cat(sprintf(" \u2713 (%.0fs)\n",
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      }, error = function(e) {
        cat(sprintf(" \u2717 %s\n", e$message))
        failed <<- c(failed, paste0("persona:", pname))
      })
    }
  }
  
  # ── Summary ────────────────────────────────────────────────────────
  if (length(failed) > 0) {
    cat(sprintf("\n\u2717 %d failure(s): %s\n",
                length(failed), paste(failed, collapse = ", ")))
  } else {
    cat(sprintf("\n\u2713 All scoring complete for %s\n", email))
  }
  
  return(invisible(list(
    login_id = login_id,
    email    = email,
    failed   = failed
  )))
}

# email <-  "link1.kate.shaw@living-group.com"
# email2 <-  "link2.kate.shaw@living-group.com"
# 

# rename_linked_account(email,"Top 25 US UHNW Wealth Management Firms")
# 
# set_extra_competitors(email,30)
# 
# rescore_user_direct(email2,
#                     include_brands   = TRUE,
#                     include_prompts  = FALSE,
#                     include_personas = FALSE,
#                     model            = "gpt-4.1-mini")

