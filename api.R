library(plumber)
library(DBI)
library(RPostgres)
library(dplyr)
library(jsonlite)

# ── DB connection ──────────────────────────────────────────────────────────
make_api_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    port     = Sys.getenv("DB_PORT"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

# ── Auth filter — runs before every endpoint ───────────────────────────────
#* @filter auth
function(req, res) {
  
  api_key <- req$HTTP_X_API_KEY
  
  if (is.null(api_key) || !nzchar(api_key)) {
    res$status <- 401
    return(list(
      error   = "Unauthorised",
      message = "Provide your API key in the X-API-Key header."
    ))
  }
  
  con <- make_api_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  user <- tryCatch({
    result <- dbGetQuery(con, "
      SELECT k.login_id, k.api_key_id, u.email,
             ds.subscription_name
      FROM dim_api_keys k
      JOIN dim_user u ON u.login_id = k.login_id
      JOIN fact_user_sub_level fus ON fus.login_id = k.login_id
        AND fus.date_valid_from <= CURRENT_DATE
        AND fus.date_valid_to >= CURRENT_DATE
      JOIN dim_subscription ds
        ON ds.subscription_level_id = fus.subscription_level_id
      WHERE k.api_key = $1 AND k.is_active = TRUE",
                         params = list(api_key))
    
    if (nrow(result) == 0) return(NULL)
    if (result$subscription_name[1] != "Enterprise") return(NULL)
    
    dbExecute(con, "
      UPDATE dim_api_keys SET date_last_used = NOW()
      WHERE api_key_id = $1",
              params = list(result$api_key_id[1]))
    
    result
  }, error = function(e) NULL)
  
  if (is.null(user)) {
    res$status <- 403
    return(list(
      error   = "Forbidden",
      message = "Invalid or inactive API key, or non-Enterprise account."
    ))
  }
  
  # Attach user to request so endpoints can access it
  req$auth_login_id <- user$login_id[1]
  req$auth_email    <- user$email[1]
  
  plumber::forward()
}

# ============================================
# ENDPOINT 1: Brand data
# GET /v1/brand-scores
# ============================================

#* Returns all brand-level scores for the authenticated user,
#* including persona splits if available.
#*
#* @param from  Start date (YYYY-MM-DD). Optional, defaults to 90 days ago.
#* @param to    End date   (YYYY-MM-DD). Optional, defaults to today.
#* @param brand Filter to a specific brand name. Optional.
#*
#* @get /v1/brand-scores
#* @serializer json
function(req, res, from = NULL, to = NULL, brand = NULL) {
  
  login_id <- req$auth_login_id
  
  con <- make_api_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Date defaults
  date_from <- if (is.null(from) || !nzchar(from)) {
    Sys.Date() - 90
  } else {
    tryCatch(as.Date(from), error = function(e) Sys.Date() - 90)
  }
  
  date_to <- if (is.null(to) || !nzchar(to)) {
    Sys.Date()
  } else {
    tryCatch(as.Date(to), error = function(e) Sys.Date())
  }
  
  # ── Overall brand scores ──────────────────────────────────────────────
  overall <- tryCatch({
    dbGetQuery(con, "
      SELECT
        b.brand_name,
        ubt.main_brand_flag,
        fa.date,
        fa.airr_score,
        fpres.overall_score   AS presence_score,
        fperc.perception_score,
        fprest.prestige_score,
        fpers.persistence_score
      FROM fact_user_brands_tracked ubt
      JOIN dim_brand b ON b.brand_id = ubt.brand_id
      JOIN fact_airr_history fa
        ON fa.brand_id = ubt.brand_id AND fa.login_id = $1
      LEFT JOIN fact_presence_history fpres
        ON fpres.brand_id = ubt.brand_id
        AND fpres.date = fa.date AND fpres.login_id = $1
      LEFT JOIN fact_perception_history fperc
        ON fperc.brand_id = ubt.brand_id
        AND fperc.date = fa.date AND fperc.login_id = $1
      LEFT JOIN fact_prestige_history fprest
        ON fprest.brand_id = ubt.brand_id
        AND fprest.date = fa.date AND fprest.login_id = $1
      LEFT JOIN fact_persistence_history fpers
        ON fpers.brand_id = ubt.brand_id
        AND fpers.date = fa.date AND fpers.login_id = $1
      WHERE ubt.login_id = $1
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
        AND fa.date BETWEEN $2 AND $3
      ORDER BY b.brand_name, fa.date",
               params = list(login_id, date_from, date_to))
  }, error = function(e) data.frame())
  
  # Optional brand filter
  if (!is.null(brand) && nzchar(brand) && nrow(overall) > 0) {
    overall <- overall[tolower(overall$brand_name) == tolower(brand), ]
  }
  
  # ── Persona brand scores ──────────────────────────────────────────────
  personas <- tryCatch({
    dbGetQuery(con, "
      SELECT
        dcp.profile_name  AS persona,
        b.brand_name,
        ubt.main_brand_flag,
        fpbh.date,
        fpbh.airr_score,
        fpbh.presence_score,
        fpbh.perception_score,
        fpbh.prestige_score,
        fpbh.persistence_score
      FROM fact_user_profiles_tracked upt
      JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
      JOIN fact_profile_brand_history fpbh ON fpbh.profile_id = upt.profile_id
      JOIN dim_brand b ON b.brand_id = fpbh.brand_id
      JOIN fact_user_brands_tracked ubt
        ON ubt.brand_id = fpbh.brand_id AND ubt.login_id = $1
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
      WHERE upt.login_id = $1
        AND upt.date_valid_from <= CURRENT_DATE
        AND (upt.date_valid_to IS NULL OR upt.date_valid_to >= CURRENT_DATE)
        AND fpbh.date BETWEEN $2 AND $3
      ORDER BY dcp.profile_name, b.brand_name, fpbh.date",
               params = list(login_id, date_from, date_to))
  }, error = function(e) data.frame())
  
  if (!is.null(brand) && nzchar(brand) && nrow(personas) > 0) {
    personas <- personas[tolower(personas$brand_name) == tolower(brand), ]
  }
  
  # ── Response ──────────────────────────────────────────────────────────
  list(
    meta = list(
      login_id  = login_id,
      date_from = format(date_from),
      date_to   = format(date_to),
      generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    overall_scores  = overall,
    persona_scores  = personas
  )
}

# ============================================
# ENDPOINT 2: Prompt data
# GET /v1/prompt-scores
# ============================================

#* Returns all prompt-level scores for the authenticated user,
#* including persona splits if available.
#*
#* @param from   Start date (YYYY-MM-DD). Optional, defaults to 90 days ago.
#* @param to     End date   (YYYY-MM-DD). Optional, defaults to today.
#* @param prompt Filter to a specific prompt string. Optional.
#* @param brand  Filter to a specific brand name. Optional.
#*
#* @get /v1/prompt-scores
#* @serializer json
function(req, res, from = NULL, to = NULL, prompt = NULL, brand = NULL) {
  
  login_id <- req$auth_login_id
  
  con <- make_api_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  date_from <- if (is.null(from) || !nzchar(from)) {
    Sys.Date() - 90
  } else {
    tryCatch(as.Date(from), error = function(e) Sys.Date() - 90)
  }
  
  date_to <- if (is.null(to) || !nzchar(to)) {
    Sys.Date()
  } else {
    tryCatch(as.Date(to), error = function(e) Sys.Date())
  }
  
  # ── Overall prompt scores ─────────────────────────────────────────────
  overall <- tryCatch({
    dbGetQuery(con, "
      SELECT
        dq.query_string     AS prompt,
        b.brand_name,
        ubt.main_brand_flag,
        fqh.date,
        fqh.airr_score,
        fqh.presence_score,
        fqh.perception_score,
        fqh.prestige_score,
        fqh.persistence_score
      FROM fact_user_queries_tracked uqt
      JOIN dim_query dq ON dq.query_id = uqt.query_id
      JOIN fact_query_history fqh ON fqh.query_id = uqt.query_id
      JOIN dim_brand b ON b.brand_id = fqh.brand_id
      JOIN fact_user_brands_tracked ubt
        ON ubt.brand_id = fqh.brand_id AND ubt.login_id = $1
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
      WHERE uqt.login_id = $1
        AND uqt.date_valid_from <= CURRENT_DATE
        AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)
        AND fqh.date BETWEEN $2 AND $3
      ORDER BY dq.query_string, b.brand_name, fqh.date",
               params = list(login_id, date_from, date_to))
  }, error = function(e) data.frame())
  
  # Optional filters
  if (!is.null(prompt) && nzchar(prompt) && nrow(overall) > 0) {
    overall <- overall[tolower(overall$prompt) == tolower(prompt), ]
  }
  if (!is.null(brand) && nzchar(brand) && nrow(overall) > 0) {
    overall <- overall[tolower(overall$brand_name) == tolower(brand), ]
  }
  
  # ── Persona prompt scores ─────────────────────────────────────────────
  personas <- tryCatch({
    dbGetQuery(con, "
      SELECT
        dcp.profile_name  AS persona,
        dq.query_string   AS prompt,
        b.brand_name,
        ubt.main_brand_flag,
        fpqh.date,
        fpqh.airr_score,
        fpqh.presence_score,
        fpqh.perception_score,
        fpqh.prestige_score,
        fpqh.persistence_score
      FROM fact_user_profiles_tracked upt
      JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
      JOIN fact_profile_query_history fpqh ON fpqh.profile_id = upt.profile_id
      JOIN dim_query dq ON dq.query_id = fpqh.query_id
      JOIN fact_user_queries_tracked uqt
        ON uqt.query_id = fpqh.query_id AND uqt.login_id = $1
        AND uqt.date_valid_from <= CURRENT_DATE
        AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)
      JOIN dim_brand b ON b.brand_id = fpqh.brand_id
      JOIN fact_user_brands_tracked ubt
        ON ubt.brand_id = fpqh.brand_id AND ubt.login_id = $1
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
      WHERE upt.login_id = $1
        AND upt.date_valid_from <= CURRENT_DATE
        AND (upt.date_valid_to IS NULL OR upt.date_valid_to >= CURRENT_DATE)
        AND fpqh.date BETWEEN $2 AND $3
      ORDER BY dcp.profile_name, dq.query_string, b.brand_name, fpqh.date",
               params = list(login_id, date_from, date_to))
  }, error = function(e) data.frame())
  
  if (!is.null(prompt) && nzchar(prompt) && nrow(personas) > 0) {
    personas <- personas[tolower(personas$prompt) == tolower(prompt), ]
  }
  if (!is.null(brand) && nzchar(brand) && nrow(personas) > 0) {
    personas <- personas[tolower(personas$brand_name) == tolower(brand), ]
  }
  
  list(
    meta = list(
      login_id  = login_id,
      date_from = format(date_from),
      date_to   = format(date_to),
      generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    overall_scores = overall,
    persona_scores = personas
  )
}