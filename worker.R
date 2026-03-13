# ============================================
# worker.R — Job queue processor
# Runs continuously on the worker server
# ============================================

library(DBI)
library(RPostgres)
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(glue)
library(lubridate)

setwd("~/airr")

source("global_scripts/app_helper_functions.R")
source("global_scripts/chatgpt_functions.R")
source("global_scripts/queries_send_return.R")
source("global_scripts/presence.R")
source("global_scripts/perception.R")
source("global_scripts/prestige.R")
source("global_scripts/persistence.R")
source("global_scripts/full_airr_score.R")
source("global_scripts/upload_functions.R")
source("global_scripts/profile_scoring.R")

message("=== AiRR Worker Started ===")
message(sprintf("Time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

poll_interval <- 5  # seconds between checks when idle

while (TRUE) {
  
  con <- NULL
  
  tryCatch({
    con <- make_con()
    
    # Claim the next pending job (atomic — prevents two workers grabbing the same job)
    job <- dbGetQuery(con, "
      UPDATE dim_job_queue
      SET status = 'running', started_at = NOW()
      WHERE job_id = (
        SELECT job_id FROM dim_job_queue
        WHERE status = 'pending'
        ORDER BY created_at
        LIMIT 1
        FOR UPDATE SKIP LOCKED
      )
      RETURNING *")
    
    if (nrow(job) == 0) {
      dbDisconnect(con)
      Sys.sleep(poll_interval)
      next
    }
    
    job_id   <- job$job_id[1]
    job_type <- job$job_type[1]
    lid      <- job$login_id[1]
    payload  <- fromJSON(job$payload[1])
    
    message(sprintf("\n=== JOB %d: %s (user %d) [%s] ===",
                    job_id, job_type, lid,
                    format(Sys.time(), "%H:%M:%S")))
    
    t0 <- Sys.time()
    
    switch(job_type,
           
           # ---- Score a single brand (main or competitor) ----
           "score_brand" = ,
           "score_competitor" = {
             brand_name <- payload$brand_name
             if (is.null(brand_name) || !nzchar(brand_name)) {
               brand_name <- dbGetQuery(con,
                                        "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                                        params = list(payload$brand_id))$brand_name[1]
             }
             
             message(sprintf("  Scoring brand: %s (ID: %s)", brand_name, payload$brand_id))
             user_create_airr(con, brand_name, lid)
             
             if (!is.null(payload$prompts) && length(payload$prompts) > 0) {
               prompt_names <- names(payload$prompts)
               for (pt in prompt_names) {
                 qid <- payload$prompts[[pt]]
                 message(sprintf("    Prompt: %s (ID: %s)", substr(pt, 1, 50), qid))
                 tryCatch(
                   create_prompt_airr(con, payload$brand_id, pt, qid, lid),
                   error = function(e) message(sprintf("    ✗ Prompt failed: %s", e$message))
                 )
               }
             }
           },
           
           # ---- Score a brand + its prompts ----
           "score_brand_prompts" = {
             # Look up brand_name if not in payload
             brand_name <- payload$brand_name
             if (is.null(brand_name) || !nzchar(brand_name)) {
               brand_name <- dbGetQuery(con,
                                        "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                                        params = list(payload$brand_id))$brand_name[1]
             }
             
             message(sprintf("  Scoring brand + prompts: %s (ID: %s)",
                             brand_name, payload$brand_id))
             user_create_airr(con, brand_name, lid)
             
             if (!is.null(payload$prompts) && length(payload$prompts) > 0) {
               prompt_names <- names(payload$prompts)
               for (pt in prompt_names) {
                 qid <- payload$prompts[[pt]]
                 message(sprintf("    Prompt: %s (ID: %s)", substr(pt, 1, 50), qid))
                 tryCatch(
                   create_prompt_airr(con, payload$brand_id, pt, qid, lid),
                   error = function(e) message(sprintf("    ✗ Prompt failed: %s", e$message))
                 )
               }
             }
           },
           
           # ---- Score all brands for a single query ----
           "score_query" = {
             message(sprintf("  Scoring query: %s (ID: %s) for %d brands",
                             substr(payload$query_string, 1, 50),
                             payload$query_id,
                             length(payload$brand_ids)))
             create_prompt_airr_multiple(con, payload$brand_ids,
                                         payload$query_string,
                                         payload$query_id, lid)
           },
           
           # ---- Score a customer profile ----
           "score_profile" = {
             message(sprintf("  Scoring profile: %s", payload$profile_id))
             score_profile(con, lid, payload$profile_id)
           },
           
           # ---- Unknown job type ----
           {
             stop(sprintf("Unknown job type: %s", job_type))
           }
    )
    
    elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    
    dbExecute(con, "
      UPDATE dim_job_queue
      SET status = 'complete', completed_at = NOW()
      WHERE job_id = $1",
              params = list(job_id))
    
    message(sprintf("=== JOB %d COMPLETE: %ss ===", job_id, elapsed))
    
  }, error = function(e) {
    message(sprintf("=== JOB ERROR: %s ===", e$message))
    
    # Try to mark the job as failed
    tryCatch({
      if (!is.null(con) && dbIsValid(con)) {
        # Get job_id if we have it
        if (exists("job_id") && !is.null(job_id)) {
          dbExecute(con, "
            UPDATE dim_job_queue
            SET status = 'failed', completed_at = NOW(), error_message = $1
            WHERE job_id = $2",
                    params = list(e$message, job_id))
        }
      }
    }, error = function(e2) {
      message(sprintf("  Could not update job status: %s", e2$message))
    })
  })
  
  # Always disconnect
  tryCatch({
    if (!is.null(con) && dbIsValid(con)) dbDisconnect(con)
  }, error = function(e) invisible(NULL))
}
