

# Set these login_ids to inactive — replace with your actual list
# inactive_login_ids <- c(1	,
#                         6	,
#                         13	,
#                         14	,
#                         17	,
#                         19	,
#                         20	,
#                         21	,
#                         22	,
#                         23	,
#                         24	,
#                         25	,
#                         26	,
#                         27	,
#                         31	,
#                         32	,
#                         33	,
#                         34	)  # <-- replace with real IDs



pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  # Connection-level statement timeout — kills any query running > 30s
  options  = "-c statement_timeout=30000"
)

deactivate_user <- function(inactive_login_ids){
  for (lid in inactive_login_ids) {
    
    # Close off brand tracking
    dbExecute(pool, "
    UPDATE fact_user_brands_tracked
    SET date_valid_to = CURRENT_DATE - INTERVAL '1 day'
    WHERE login_id = $1
      AND (date_valid_to IS NULL OR date_valid_to > CURRENT_DATE)",
              params = list(lid))
    
    # Close off query tracking
    dbExecute(pool, "
    UPDATE fact_user_queries_tracked
    SET date_valid_to = CURRENT_DATE - INTERVAL '1 day'
    WHERE login_id = $1
      AND (date_valid_to IS NULL OR date_valid_to > CURRENT_DATE)",
              params = list(lid))
    
    # Close off profile tracking
    dbExecute(pool, "
    UPDATE fact_user_profiles_tracked
    SET date_valid_to = CURRENT_DATE - INTERVAL '1 day'
    WHERE login_id = $1
      AND (date_valid_to IS NULL OR date_valid_to > CURRENT_DATE)",
              params = list(lid))
    
    # Close off subscription
    dbExecute(pool, "
    UPDATE fact_user_sub_level
    SET date_valid_to = CURRENT_DATE - INTERVAL '1 day'
    WHERE login_id = $1
      AND (date_valid_to IS NULL OR date_valid_to > CURRENT_DATE)",
              params = list(lid))
    
    message(sprintf("✓ Deactivated login_id %d", lid))
  }
}

example
deactivate_user(inactive_login_ids = c(40))


