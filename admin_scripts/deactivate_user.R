

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

active_users <- dbGetQuery(pool, "
  SELECT 
    u.login_id,
    u.email,
    u.date_added,
    b.brand_name,
    ds.subscription_name,
    MAX(fa.date) as last_score_date
  FROM dim_user u
  LEFT JOIN fact_user_brands_tracked ubt 
    ON ubt.login_id = u.login_id
    AND ubt.main_brand_flag = TRUE
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  LEFT JOIN dim_brand b 
    ON b.brand_id = ubt.brand_id
  LEFT JOIN fact_user_sub_level fus 
    ON fus.login_id = u.login_id
    AND fus.date_valid_from <= CURRENT_DATE
    AND fus.date_valid_to >= CURRENT_DATE
  LEFT JOIN dim_subscription ds 
    ON ds.subscription_level_id = fus.subscription_level_id
  LEFT JOIN fact_airr_history fa 
    ON fa.login_id = u.login_id
  WHERE u.onboarding_complete = TRUE
  GROUP BY u.login_id, u.email, u.date_added, b.brand_name, ds.subscription_name
  ORDER BY u.date_added DESC
")

# print(active_users)

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

# example
deactivate_user(inactive_login_ids = c(11,18,28,29))


