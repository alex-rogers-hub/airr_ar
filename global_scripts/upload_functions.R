# ============================================
# upload_functions.R
# All functions take an explicit `con` parameter
# ============================================

AIRR_WEIGHTS <- list(
  presence    = 0.25,
  perception  = 0.3,
  prestige    = 0.25,
  persistence = 0.2
)

make_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    port     = Sys.getenv("DB_PORT"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

# ============================================
# Brand helpers
# ============================================

add_brand <- function(con, brand_name) {
  result <- dbGetQuery(con,
                       "INSERT INTO dim_brand (brand_name) VALUES ($1) RETURNING brand_id",
                       params = list(brand_name))
  return(result$brand_id)
}

get_or_create_brand_id <- function(con, brand_name) {
  result <- dbGetQuery(con,
                       "SELECT brand_id FROM dim_brand WHERE lower(brand_name) = lower($1)",
                       params = list(brand_name))
  if (nrow(result) == 0) return(add_brand(con, brand_name))
  return(result$brand_id)
}

# ============================================
# Upload daily refresh
# ============================================

upload_daily_refresh <- function(con, brand_name, login_id,
                                 input_data, run_date) {
  dbBegin(con)
  tryCatch({
    score_date <- run_date
    air_scores <- input_data
    brand_id   <- get_or_create_brand_id(con, brand_name)
    
    presence <- air_scores$presence
    dbExecute(con, "
      INSERT INTO fact_presence_history (
        brand_id, login_id, date, overall_score, simple_mention_rate,
        total_responses, responses_with_mentions, interpretation, stability
      ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)
      ON CONFLICT (brand_id, login_id, date) DO UPDATE SET
        overall_score           = EXCLUDED.overall_score,
        simple_mention_rate     = EXCLUDED.simple_mention_rate,
        total_responses         = EXCLUDED.total_responses,
        responses_with_mentions = EXCLUDED.responses_with_mentions,
        interpretation          = EXCLUDED.interpretation,
        stability               = EXCLUDED.stability",
              params = list(
                brand_id, login_id, score_date,
                presence$overall_score, presence$simple_mention_rate,
                presence$total_responses, presence$responses_with_mentions,
                presence$interpretation, presence$stability
              ))
    message(sprintf("  \u2713 Presence: %s / user %d", brand_name, login_id))
    
    perception <- air_scores$perception
    dbExecute(con, "
      INSERT INTO fact_perception_history (
        brand_id, login_id, date, perception_score,
        perception_accuracy_score, perception_sentiment_score,
        prestige_accuracy_interpretation
      ) VALUES ($1,$2,$3,$4,$5,$6,$7)
      ON CONFLICT (brand_id, login_id, date) DO UPDATE SET
        perception_score                 = EXCLUDED.perception_score,
        perception_accuracy_score        = EXCLUDED.perception_accuracy_score,
        perception_sentiment_score       = EXCLUDED.perception_sentiment_score,
        prestige_accuracy_interpretation = EXCLUDED.prestige_accuracy_interpretation",
              params = list(
                brand_id, login_id, score_date,
                perception$perception_score, perception$perception_accuracy_score,
                perception$perception_sentiment_score,
                perception$prestige_accuracy_interpretation
              ))
    message(sprintf("  \u2713 Perception: %s / user %d", brand_name, login_id))
    
    prestige <- air_scores$prestige
    dbExecute(con, "
      INSERT INTO fact_prestige_history (
        brand_id, login_id, date, prestige_score, prestige_rank_score,
        prestige_rank_comps_brands, prestige_authority_score,
        prestige_leadership_score
      ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
      ON CONFLICT (brand_id, login_id, date) DO UPDATE SET
        prestige_score             = EXCLUDED.prestige_score,
        prestige_rank_score        = EXCLUDED.prestige_rank_score,
        prestige_rank_comps_brands = EXCLUDED.prestige_rank_comps_brands,
        prestige_authority_score   = EXCLUDED.prestige_authority_score,
        prestige_leadership_score  = EXCLUDED.prestige_leadership_score",
              params = list(
                brand_id, login_id, score_date,
                prestige$prestige_score, prestige$prestige_rank_score,
                paste(prestige$prestige_rank_comps$brand, collapse = " | "),
                prestige$prestige_authority_score, prestige$prestige_leadership_score
              ))
    message(sprintf("  \u2713 Prestige: %s / user %d", brand_name, login_id))
    
    dbCommit(con)
    message(sprintf("\u2713\u2713 All scores inserted: %s / user %d",
                    brand_name, login_id))
    return(TRUE)
    
  }, error = function(e) {
    dbRollback(con)
    warning(sprintf("\u2717 Upload failed for %s / user %d: %s",
                    brand_name, login_id, e$message))
    return(FALSE)
  })
}

# ============================================
# Persistence
# ============================================

calc_daily_persistance <- function(con, brand_name, login_id, run_date) {
  score_date  <- run_date
  persistence <- calculate_daily_persistence(brand_name, login_id,
                                             score_date, con)
  brand_id    <- get_or_create_brand_id(con, brand_name)
  
  dbBegin(con)
  tryCatch({
    dbExecute(con, "
      INSERT INTO fact_persistence_history (
        brand_id, login_id, date, persistence_score,
        coefficient_of_variation, interpretation, daily_perc_change
      ) VALUES ($1,$2,$3,$4,$5,$6,$7)
      ON CONFLICT (brand_id, login_id, date) DO UPDATE SET
        persistence_score        = EXCLUDED.persistence_score,
        coefficient_of_variation = EXCLUDED.coefficient_of_variation,
        interpretation           = EXCLUDED.interpretation,
        daily_perc_change        = EXCLUDED.daily_perc_change",
              params = list(
                brand_id, login_id, score_date,
                persistence$score, persistence$coefficient_of_variation,
                persistence$interpretation, persistence$daily_perc_change
              ))
    dbCommit(con)
    message(sprintf("  \u2713 Persistence: %s / user %d", brand_name, login_id))
  }, error = function(e) {
    dbRollback(con)
    warning(sprintf("\u2717 Persistence failed for %s / user %d: %s",
                    brand_name, login_id, e$message))
  })
}

# ============================================
# AIRR score
# ============================================

calc_daily_airr <- function(con, brand_name, login_id, run_date) {
  score_date <- run_date
  brand_id   <- get_or_create_brand_id(con, brand_name)
  
  persistence <- dbGetQuery(con,
                            "SELECT persistence_score FROM fact_persistence_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3",
                            params = list(brand_id, login_id, score_date))$persistence_score
  
  presence <- dbGetQuery(con,
                         "SELECT overall_score FROM fact_presence_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3",
                         params = list(brand_id, login_id, score_date))$overall_score
  
  perception <- dbGetQuery(con,
                           "SELECT perception_score FROM fact_perception_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3",
                           params = list(brand_id, login_id, score_date))$perception_score
  
  prestige <- dbGetQuery(con,
                         "SELECT prestige_score FROM fact_prestige_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3",
                         params = list(brand_id, login_id, score_date))$prestige_score
  
  airr_score <- perception  * AIRR_WEIGHTS$perception  +
    presence    * AIRR_WEIGHTS$presence    +
    prestige    * AIRR_WEIGHTS$prestige    +
    persistence * AIRR_WEIGHTS$persistence
  
  dbBegin(con)
  tryCatch({
    dbExecute(con, "
      INSERT INTO fact_airr_history (brand_id, login_id, date, airr_score)
      VALUES ($1,$2,$3,$4)
      ON CONFLICT (brand_id, login_id, date) DO UPDATE SET
        airr_score = EXCLUDED.airr_score",
              params = list(brand_id, login_id, score_date, airr_score))
    dbCommit(con)
    message(sprintf("  \u2713 AIRR: %s / user %d = %.1f",
                    brand_name, login_id, airr_score))
  }, error = function(e) {
    dbRollback(con)
    warning(sprintf("\u2717 AIRR insert failed for %s / user %d: %s",
                    brand_name, login_id, e$message))
  })
}

# ============================================
# user_create_airr
# ============================================

user_create_airr <- function(con, brand_name, login_id,
                             model = "gpt-4o-mini") {
  message(sprintf("=== START user_create_airr: %s / user %d [%s] ===",
                  brand_name, login_id, format(Sys.time(), "%H:%M:%S")))
  brand_id <- get_or_create_brand_id(con, brand_name)
  
  
  brand_info <- dbGetQuery(con,
                           "SELECT b.brand_reach, b.reach_country, b.reach_region, b.reach_postcode,
            ubt.industry
     FROM dim_brand b
     JOIN fact_user_brands_tracked ubt ON ubt.brand_id = b.brand_id
     WHERE b.brand_id = $1 AND ubt.login_id = $2
       AND ubt.date_valid_from <= CURRENT_DATE
       AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
     LIMIT 1",
                           params = list(brand_id, login_id))
  
  if (nrow(brand_info) > 0) {
    reach    <- brand_info$brand_reach[1] %||% "global"
    country  <- brand_info$reach_country[1]
    region   <- brand_info$reach_region[1]
    postcode <- brand_info$reach_postcode[1]
    industry <- brand_info$industry[1]
    if (is.na(country))  country  <- NULL
    if (is.na(region))   region   <- NULL
    if (is.na(postcode)) postcode <- NULL
    if (is.na(industry)) industry <- NULL
  } else {
    reach <- "global"; country <- NULL; region <- NULL
    postcode <- NULL; industry <- NULL
  }
  
  message(sprintf("  Fetching AI responses for %s...", brand_name))
  airr_scores <- full_air_score(brand_name, model,
                                industry       = industry,
                                brand_reach    = reach,
                                reach_country  = country,
                                reach_region   = region,
                                reach_postcode = postcode,
                                brand_id       = brand_id,  # new
                                con            = con)  
  
  message(sprintf("  Uploading scores for %s...", brand_name))
  upload_daily_refresh(con, brand_name, login_id, airr_scores,
                       as.Date(Sys.Date()))
  calc_daily_persistance(con, brand_name, login_id, Sys.Date())
  calc_daily_airr(con, brand_name, login_id, Sys.Date())
  
  message(sprintf("=== END user_create_airr: %s / user %d [%s] ===",
                  brand_name, login_id,
                  format(Sys.time(), "%H:%M:%S")))
}

# ============================================
# create_prompt_airr — uses aliases
# ============================================

create_prompt_airr <- function(con, brand_id, query_string, query_id_in,
                               login_id, model = "gpt-4o-mini") {
  
  brand_meta <- dbGetQuery(con,
                           "SELECT brand_name, brand_reach, reach_country,
            reach_region, reach_postcode
     FROM dim_brand WHERE brand_id = $1",
                           params = list(brand_id))
  
  brand_name  <- brand_meta$brand_name[1]
  reach       <- brand_meta$brand_reach[1] %||% "global"
  nm_country  <- brand_meta$reach_country[1]
  nm_region   <- brand_meta$reach_region[1]
  nm_postcode <- brand_meta$reach_postcode[1]
  
  # Fetch aliases
  search_names <- get_brand_search_names(brand_id, brand_name, con)
  message(sprintf("  Search names: %s", paste(search_names, collapse = ", ")))
  
  message(sprintf("=== START create_prompt_airr: %s / user %d [%s] ===",
                  brand_name, login_id,
                  format(Sys.time(), "%H:%M:%S")))
  
  if (!is.null(nm_country)  && is.na(nm_country))  nm_country  <- NULL
  if (!is.null(nm_region)   && is.na(nm_region))   nm_region   <- NULL
  if (!is.null(nm_postcode) && is.na(nm_postcode)) nm_postcode <- NULL
  
  query_string_resolved <- inject_near_me_location(
    query_string, reach, nm_country, nm_region, nm_postcode)
  
  message(sprintf("  Resolved prompt: %s", query_string_resolved))
  message(sprintf("  Fetching prompt responses for %s...", brand_name))
  rel_responses <- prompt_queries(rep(query_string_resolved, 10),
                                  model = model)
  
  dbExecute(con,
            "INSERT INTO dim_brand_query (brand_id, query_id, date_added)
     VALUES ($1,$2,$3) ON CONFLICT (brand_id, query_id) DO NOTHING",
            params = list(brand_id, query_id_in, Sys.Date()))
  
  # Use search_names (includes aliases) for presence detection
  presence_prompt_score   <- presence_prompt_calc(search_names, rel_responses)
  prestige_prompt_score   <- calculate_prestige_from_prompts_sep(
    brand_name    = brand_name,
    brand_id      = brand_id,
    login_id      = login_id,
    rel_responses = rel_responses,
    db_con        = con
  )
  perception_prompt_score <- calculate_perception_from_prompts_sep(
    brand_name, brand_id, rel_responses)
  
  dbExecute(con, "
    INSERT INTO fact_query_history (
      brand_id, query_id, date, presence_score, perception_score,
      prestige_score, persistence_score, airr_score
    ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
    ON CONFLICT (brand_id, query_id, date) DO UPDATE SET
      presence_score    = EXCLUDED.presence_score,
      perception_score  = EXCLUDED.perception_score,
      prestige_score    = EXCLUDED.prestige_score,
      persistence_score = EXCLUDED.persistence_score,
      airr_score        = EXCLUDED.airr_score",
            params = list(brand_id, query_id_in, Sys.Date(),
                          presence_prompt_score, perception_prompt_score,
                          prestige_prompt_score, 0, 0))
  
  presence_history <- dbGetQuery(con,
                                 "SELECT date, presence_score FROM fact_query_history
     WHERE brand_id = $1 AND date <= $2 AND query_id = $3
     ORDER BY date",
                                 params = list(brand_id, Sys.Date(), query_id_in))
  
  persistence_prompt_score <- if (nrow(presence_history) < 5) {
    presence_prompt_score
  } else {
    calculate_daily_persistence_sep(presence_history)
  }
  
  todays_scores <- dbGetQuery(con,
                              "SELECT presence_score, perception_score, prestige_score
     FROM fact_query_history
     WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                              params = list(brand_id, Sys.Date(), query_id_in))
  
  airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception +
    todays_scores$presence_score   * AIRR_WEIGHTS$presence   +
    todays_scores$prestige_score   * AIRR_WEIGHTS$prestige   +
    persistence_prompt_score       * AIRR_WEIGHTS$persistence
  
  dbExecute(con,
            "UPDATE fact_query_history
     SET persistence_score = $1, airr_score = $2
     WHERE brand_id = $3 AND date = $4 AND query_id = $5",
            params = list(persistence_prompt_score, airr_score,
                          brand_id, Sys.Date(), query_id_in))
  
  message(sprintf("  \u2713 Prompt AIRR: brand %d, query %d",
                  brand_id, query_id_in))
  message(sprintf("=== END create_prompt_airr: %s / user %d [%s] ===",
                  brand_name, login_id,
                  format(Sys.time(), "%H:%M:%S")))
}

# ============================================
# create_prompt_airr_multiple — uses aliases
# ============================================

create_prompt_airr_multiple <- function(con, brand_ids, query_string,
                                        query_id_in, login_id,
                                        model = "gpt-4o-mini") {
  
  message(sprintf("=== create_prompt_airr_multiple: %d brands ===",
                  length(brand_ids)))
  
  placeholders   <- paste0("$", seq_along(brand_ids), collapse = ", ")
  all_brand_meta <- dbGetQuery(con,
                               sprintf("SELECT brand_id, brand_name, brand_reach,
                    reach_country, reach_region, reach_postcode
             FROM dim_brand WHERE brand_id IN (%s)", placeholders),
                               params = as.list(brand_ids))
  
  all_brand_meta$query_resolved <- mapply(function(reach, country,
                                                   region, postcode) {
    country  <- if (is.na(country))  NULL else country
    region   <- if (is.na(region))   NULL else region
    postcode <- if (is.na(postcode)) NULL else postcode
    inject_near_me_location(query_string, reach, country, region, postcode)
  },
  all_brand_meta$brand_reach,
  all_brand_meta$reach_country,
  all_brand_meta$reach_region,
  all_brand_meta$reach_postcode)
  
  unique_resolved <- unique(all_brand_meta$query_resolved)
  responses_cache <- list()
  for (uq in unique_resolved) {
    message(sprintf("  Fetching responses for: %s", substr(uq, 1, 60)))
    responses_cache[[uq]] <- prompt_queries(rep(uq, 10), model = model)
  }
  
  for (bid in brand_ids) {
    dbExecute(con,
              "INSERT INTO dim_brand_query (brand_id, query_id, date_added)
       VALUES ($1,$2,$3) ON CONFLICT (brand_id, query_id) DO NOTHING",
              params = list(bid, query_id_in, Sys.Date()))
  }
  
  for (idx in seq_along(brand_ids)) {
    bid  <- brand_ids[idx]
    meta <- all_brand_meta[all_brand_meta$brand_id == bid, ]
    
    if (nrow(meta) == 0) {
      warning(sprintf("No metadata for brand_id %d, skipping", bid))
      next
    }
    
    brand_name    <- meta$brand_name[1]
    rel_responses <- responses_cache[[meta$query_resolved[1]]]
    
    # Fetch aliases for presence detection
    search_names <- get_brand_search_names(bid, brand_name, con)
    
    message(sprintf("  [%d/%d] Scoring %s (aliases: %s)...",
                    idx, length(brand_ids), brand_name,
                    paste(search_names, collapse = ", ")))
    
    presence_prompt_score   <- presence_prompt_calc(search_names, rel_responses)
    prestige_prompt_score   <- calculate_prestige_from_prompts_sep(
      brand_name    = brand_name,
      brand_id      = bid,
      login_id      = login_id,
      rel_responses = rel_responses,
      db_con        = con
    )
    perception_prompt_score <- calculate_perception_from_prompts_sep(
      brand_name, bid, rel_responses)
    
    dbExecute(con, "
      INSERT INTO fact_query_history (
        brand_id, query_id, date, presence_score, perception_score,
        prestige_score, persistence_score, airr_score
      ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
      ON CONFLICT (brand_id, query_id, date) DO UPDATE SET
        presence_score    = EXCLUDED.presence_score,
        perception_score  = EXCLUDED.perception_score,
        prestige_score    = EXCLUDED.prestige_score,
        persistence_score = EXCLUDED.persistence_score,
        airr_score        = EXCLUDED.airr_score",
              params = list(bid, query_id_in, Sys.Date(),
                            presence_prompt_score, perception_prompt_score,
                            prestige_prompt_score, 0, 0))
    
    ph <- dbGetQuery(con,
                     "SELECT date, presence_score FROM fact_query_history
       WHERE brand_id = $1 AND date <= $2 AND query_id = $3
       ORDER BY date",
                     params = list(bid, Sys.Date(), query_id_in))
    
    persistence_prompt_score <- if (nrow(ph) < 5) presence_prompt_score else
      calculate_daily_persistence_sep(ph)
    
    ts <- dbGetQuery(con,
                     "SELECT presence_score, perception_score, prestige_score
       FROM fact_query_history
       WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                     params = list(bid, Sys.Date(), query_id_in))
    
    airr <- ts$perception_score * AIRR_WEIGHTS$perception +
      ts$presence_score   * AIRR_WEIGHTS$presence   +
      ts$prestige_score   * AIRR_WEIGHTS$prestige   +
      persistence_prompt_score * AIRR_WEIGHTS$persistence
    
    dbExecute(con,
              "UPDATE fact_query_history
       SET persistence_score = $1, airr_score = $2
       WHERE brand_id = $3 AND date = $4 AND query_id = $5",
              params = list(persistence_prompt_score, airr,
                            bid, Sys.Date(), query_id_in))
    
    message(sprintf("    \u2713 %s done", brand_name))
  }
  
  message("=== create_prompt_airr_multiple completed ===")
}

# ============================================
# DAILY REFRESH LOOP
# ============================================

daily_refresh_loop <- function(model = "gpt-4o-mini") {
  
  con <- make_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  user_brand_list <- dbGetQuery(con, "
    SELECT ubt.login_id, b.brand_id, b.brand_name, ubt.industry
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
    ORDER BY ubt.login_id, b.brand_name")
  
  message(sprintf("=== Daily Refresh: %d active user-brand pairs ===",
                  nrow(user_brand_list)))
  
  failed <- c()
  
  for (i in seq_len(nrow(user_brand_list))) {
    brand <- user_brand_list$brand_name[i]
    lid   <- user_brand_list$login_id[i]
    
    tryCatch({
      t0 <- Sys.time()
      message(sprintf("\n[%d/%d] %s / user %d...",
                      i, nrow(user_brand_list), brand, lid))
      user_create_airr(con, brand, lid, model)
      message(sprintf("  \u2713 Done in %.1fs",
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }, error = function(e) {
      failed <<- c(failed, paste0(brand, "/user:", lid))
      message(sprintf("  \u2717 Failed: %s", e$message))
    })
  }
  
  # ── Enterprise profiles ──────────────────────────────────────────────
  message("\n=== Scoring customer profiles ===")
  
  active_profiles <- dbGetQuery(con, "
    SELECT DISTINCT upt.login_id, upt.profile_id, dcp.profile_name
    FROM fact_user_profiles_tracked upt
    JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
    JOIN fact_user_sub_level fus ON fus.login_id = upt.login_id
    JOIN dim_subscription ds ON ds.subscription_level_id = fus.subscription_level_id
    WHERE upt.date_valid_from <= CURRENT_DATE
      AND (upt.date_valid_to IS NULL OR upt.date_valid_to > CURRENT_DATE)
      AND fus.date_valid_from <= CURRENT_DATE
      AND (fus.date_valid_to IS NULL OR fus.date_valid_to > CURRENT_DATE)
    ORDER BY upt.login_id, upt.profile_id")
  
  if (nrow(active_profiles) == 0) {
    message("No active profiles.")
  } else {
    profile_failed <- c()
    for (p in seq_len(nrow(active_profiles))) {
      lid   <- active_profiles$login_id[p]
      pid   <- active_profiles$profile_id[p]
      pname <- active_profiles$profile_name[p]
      tryCatch({
        t0 <- Sys.time()
        message(sprintf("\n  [%d/%d] %s (login: %d)...",
                        p, nrow(active_profiles), pname, lid))
        score_profile(con, lid, pid, model)
        message(sprintf("  \u2713 Done in %.1fs",
                        as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      }, error = function(e) {
        profile_failed <<- c(profile_failed, pname)
        message(sprintf("  \u2717 Failed: %s", e$message))
      })
    }
    if (length(profile_failed) > 0) {
      message(sprintf("\nProfile failures: %s",
                      paste(profile_failed, collapse = ", ")))
    }
  }
  
  if (length(failed) > 0) {
    message(sprintf("\n%d brand failures: %s",
                    length(failed), paste(failed, collapse = ", ")))
  } else {
    message("\nAll brands completed!")
  }
  
  return(invisible(failed))
}

# ============================================
# DAILY PROMPT LOOP — uses aliases
# ============================================

daily_prompt_loop <- function(model = "gpt-4o-mini") {
  
  con <- make_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  active_queries <- dbGetQuery(con, "
    SELECT DISTINCT dq.query_id, dq.query_string
    FROM dim_query dq
    INNER JOIN fact_user_queries_tracked uqt ON dq.query_id = uqt.query_id
    WHERE uqt.date_valid_from <= CURRENT_DATE
      AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
    ORDER BY dq.query_id")
  
  if (nrow(active_queries) == 0) {
    message("No active queries.")
    return(invisible(c()))
  }
  
  message(sprintf("=== Daily Prompt Loop: %d queries ===",
                  nrow(active_queries)))
  failed <- c()
  
  for (q in seq_len(nrow(active_queries))) {
    qid     <- active_queries$query_id[q]
    qstring <- active_queries$query_string[q]
    
    tryCatch({
      message(sprintf("\n[%d/%d] Query: %s (ID: %d)...",
                      q, nrow(active_queries),
                      substr(qstring, 1, 50), qid))
      
      brands_for_query <- dbGetQuery(con, "
        SELECT DISTINCT b.brand_id, b.brand_name,
               b.brand_reach, b.reach_country, b.reach_region,
               b.reach_postcode, ubt.login_id
        FROM fact_user_queries_tracked uqt
        JOIN fact_user_brands_tracked ubt
          ON uqt.login_id = ubt.login_id
        JOIN dim_brand b ON b.brand_id = ubt.brand_id
        JOIN dim_brand_query dbq
          ON dbq.brand_id = b.brand_id AND dbq.query_id = uqt.query_id
        WHERE uqt.query_id = $1
          AND uqt.date_valid_from <= CURRENT_DATE
          AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
          AND ubt.date_valid_from <= CURRENT_DATE
          AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
        ORDER BY b.brand_id, ubt.login_id",
                                     params = list(qid))
      
      if (nrow(brands_for_query) == 0) {
        message("  No active brands, skipping.")
        next
      }
      
      message(sprintf("  %d brand/user combinations to score",
                      nrow(brands_for_query)))
      
      brands_for_query$query_resolved <- mapply(
        function(reach, country, region, postcode) {
          country  <- if (is.na(country))  NULL else country
          region   <- if (is.na(region))   NULL else region
          postcode <- if (is.na(postcode)) NULL else postcode
          inject_near_me_location(qstring, reach, country, region, postcode)
        },
        brands_for_query$brand_reach,
        brands_for_query$reach_country,
        brands_for_query$reach_region,
        brands_for_query$reach_postcode
      )
      
      unique_prompts  <- unique(brands_for_query$query_resolved)
      responses_cache <- list()
      for (uq in unique_prompts) {
        message(sprintf("  Fetching responses for: %s", substr(uq, 1, 60)))
        responses_cache[[uq]] <- prompt_queries(rep(uq, 10), model = model, temperature = 0.5)
      }
      
      for (bid in unique(brands_for_query$brand_id)) {
        dbExecute(con,
                  "INSERT INTO dim_brand_query (brand_id, query_id, date_added)
           VALUES ($1,$2,$3) ON CONFLICT (brand_id, query_id) DO NOTHING",
                  params = list(bid, qid, Sys.Date()))
      }
      
      for (b in seq_len(nrow(brands_for_query))) {
        bid        <- brands_for_query$brand_id[b]
        brand_name <- brands_for_query$brand_name[b]
        lid        <- brands_for_query$login_id[b]
        rel_resp   <- responses_cache[[brands_for_query$query_resolved[b]]]
        
        # Fetch aliases for presence detection
        search_names <- get_brand_search_names(bid, brand_name, con)
        
        tryCatch({
          presence_score <- presence_prompt_calc(search_names, rel_resp)
          
          prestige_score <- calculate_prestige_from_prompts_sep(
            brand_name    = brand_name,
            brand_id      = bid,
            login_id      = lid,
            rel_responses = rel_resp,
            db_con        = con
          )
          
          perception_score <- calculate_perception_from_prompts_sep(
            brand_name, bid, rel_resp)
          
          dbExecute(con, "
            INSERT INTO fact_query_history (
              brand_id, query_id, date, presence_score, perception_score,
              prestige_score, persistence_score, airr_score
            ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
            ON CONFLICT (brand_id, query_id, date) DO UPDATE SET
              presence_score    = EXCLUDED.presence_score,
              perception_score  = EXCLUDED.perception_score,
              prestige_score    = EXCLUDED.prestige_score,
              persistence_score = EXCLUDED.persistence_score,
              airr_score        = EXCLUDED.airr_score",
                    params = list(bid, qid, Sys.Date(),
                                  presence_score, perception_score,
                                  prestige_score, 0, 0))
          
          ph <- dbGetQuery(con,
                           "SELECT date, presence_score FROM fact_query_history
             WHERE brand_id = $1 AND date <= $2 AND query_id = $3
             ORDER BY date",
                           params = list(bid, Sys.Date(), qid))
          
          persistence_score <- if (nrow(ph) < 5) presence_score else
            calculate_daily_persistence_sep(ph)
          
          ts <- dbGetQuery(con,
                           "SELECT presence_score, perception_score, prestige_score
             FROM fact_query_history
             WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                           params = list(bid, Sys.Date(), qid))
          
          airr <- ts$perception_score * AIRR_WEIGHTS$perception +
            ts$presence_score   * AIRR_WEIGHTS$presence   +
            ts$prestige_score   * AIRR_WEIGHTS$prestige   +
            persistence_score   * AIRR_WEIGHTS$persistence
          
          dbExecute(con,
                    "UPDATE fact_query_history
             SET persistence_score = $1, airr_score = $2
             WHERE brand_id = $3 AND date = $4 AND query_id = $5",
                    params = list(persistence_score, airr, bid, Sys.Date(), qid))
          
          message(sprintf("    \u2713 %s (user: %d)", brand_name, lid))
          
        }, error = function(e) {
          failed <<- c(failed,
                       paste0("brand:", bid, "/user:", lid, "/query:", qid))
          message(sprintf("    \u2717 %s: %s", brand_name, e$message))
        })
      }
      
    }, error = function(e) {
      failed <<- c(failed, paste0("query:", qid))
      message(sprintf("  \u2717 Query %d failed: %s", qid, e$message))
    })
  }
  
  if (length(failed) > 0) {
    message(sprintf("\n%d failures: %s",
                    length(failed), paste(failed, collapse = ", ")))
  } else {
    message("\nAll queries completed!")
  }
  
  return(invisible(failed))
}
