AIRR_WEIGHTS <- list(
  presence = 0.25,
  perception = 0.3,
  prestige = 0.25,
  persistence = 0.2
)

add_brand <- function(con, brand_name) {
  query <- "
    INSERT INTO dim_brand (brand_name)
    VALUES ($1)
    RETURNING brand_id;
  "
  result <- dbGetQuery(con, query, params = list(brand_name))
  return(result$brand_id)
}

get_or_create_brand_id <- function(con, brand_name) {
  result <- dbGetQuery(con,
                       "SELECT brand_id FROM dim_brand WHERE lower(brand_name) = lower($1);",
                       params = list(brand_name))
  if (nrow(result) == 0) {
    return(add_brand(con, brand_name))
  } else {
    return(result$brand_id)
  }
}

# CHANGED: added login_id parameter throughout
upload_daily_refresh <- function(brand_name, login_id, input_data, run_date) {
  
  dbBegin(con)
  
  tryCatch({
    score_date <- run_date
    air_scores <- input_data
    brand_id <- get_or_create_brand_id(con, brand_name)
    
    # ==================== INSERT PRESENCE DATA ====================
    presence <- air_scores$presence
    
    dbExecute(con, "
      INSERT INTO fact_presence_history (
        brand_id, login_id, date, overall_score, simple_mention_rate,
        total_responses, responses_with_mentions, interpretation, stability
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
      ON CONFLICT (brand_id, login_id, date) 
      DO UPDATE SET
        overall_score            = EXCLUDED.overall_score,
        simple_mention_rate      = EXCLUDED.simple_mention_rate,
        total_responses          = EXCLUDED.total_responses,
        responses_with_mentions  = EXCLUDED.responses_with_mentions,
        interpretation           = EXCLUDED.interpretation,
        stability                = EXCLUDED.stability;
    ", params = list(
      brand_id, login_id, score_date,
      presence$overall_score, presence$simple_mention_rate,
      presence$total_responses, presence$responses_with_mentions,
      presence$interpretation, presence$stability
    ))
    
    message(sprintf("✓ Inserted presence data for %s / user %d (brand ID: %d)",
                    brand_name, login_id, brand_id))
    
    # ==================== INSERT PERCEPTION DATA ====================
    perception <- air_scores$perception
    
    dbExecute(con, "
      INSERT INTO fact_perception_history (
        brand_id, login_id, date, perception_score, perception_accuracy_score,
        perception_sentiment_score, prestige_accuracy_interpretation
      ) VALUES ($1, $2, $3, $4, $5, $6, $7)
      ON CONFLICT (brand_id, login_id, date) 
      DO UPDATE SET
        perception_score                = EXCLUDED.perception_score,
        perception_accuracy_score       = EXCLUDED.perception_accuracy_score,
        perception_sentiment_score      = EXCLUDED.perception_sentiment_score,
        prestige_accuracy_interpretation = EXCLUDED.prestige_accuracy_interpretation;
    ", params = list(
      brand_id, login_id, score_date,
      perception$perception_score, perception$perception_accuracy_score,
      perception$perception_sentiment_score, perception$prestige_accuracy_interpretation
    ))
    
    message(sprintf("✓ Inserted perception data for %s / user %d (brand ID: %d)",
                    brand_name, login_id, brand_id))
    
    # ==================== INSERT PRESTIGE DATA ====================
    prestige <- air_scores$prestige
    
    dbExecute(con, "
      INSERT INTO fact_prestige_history (
        brand_id, login_id, date, prestige_score, prestige_rank_score,
        prestige_rank_comps_brands, prestige_authority_score, prestige_leadership_score
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      ON CONFLICT (brand_id, login_id, date) 
      DO UPDATE SET
        prestige_score            = EXCLUDED.prestige_score,
        prestige_rank_score       = EXCLUDED.prestige_rank_score,
        prestige_rank_comps_brands = EXCLUDED.prestige_rank_comps_brands,
        prestige_authority_score  = EXCLUDED.prestige_authority_score,
        prestige_leadership_score = EXCLUDED.prestige_leadership_score;
    ", params = list(
      brand_id, login_id, score_date,
      prestige$prestige_score, prestige$prestige_rank_score,
      paste(prestige$prestige_rank_comps$brand, collapse = " | "),
      prestige$prestige_authority_score, prestige$prestige_leadership_score
    ))
    
    message(sprintf("✓ Inserted prestige data for %s / user %d (brand ID: %d)",
                    brand_name, login_id, brand_id))
    
    dbCommit(con)
    message(sprintf("✓✓ Successfully inserted all AIR scores for %s / user %d",
                    brand_name, login_id))
    return(TRUE)
    
  }, error = function(e) {
    dbRollback(con)
    warning(sprintf("✗ Error inserting data for %s / user %d: %s",
                    brand_name, login_id, e$message))
    return(FALSE)
  })
}

# CHANGED: added login_id parameter; queries now filter by login_id too
calc_daily_persistance <- function(brand_name, login_id, run_date) {
  score_date   <- run_date
  
  # Pass login_id so it queries the right user's presence history
  persistence  <- calculate_daily_persistence(brand_name, login_id, score_date)
  brand_id     <- get_or_create_brand_id(con, brand_name)
  
  dbBegin(con)
  
  dbExecute(con, "
    INSERT INTO fact_persistence_history (
      brand_id, login_id, date, persistence_score, coefficient_of_variation,
      interpretation, daily_perc_change
    ) VALUES ($1, $2, $3, $4, $5, $6, $7)
    ON CONFLICT (brand_id, login_id, date) 
    DO UPDATE SET
      persistence_score        = EXCLUDED.persistence_score,
      coefficient_of_variation = EXCLUDED.coefficient_of_variation,
      interpretation           = EXCLUDED.interpretation,
      daily_perc_change        = EXCLUDED.daily_perc_change;
  ", params = list(
    brand_id, login_id, score_date,
    persistence$score, persistence$coefficient_of_variation,
    persistence$interpretation, persistence$daily_perc_change
  ))
  
  message(sprintf("✓ Inserted persistence data for %s / user %d (brand ID: %d)",
                  brand_name, login_id, brand_id))
  dbCommit(con)
}

# CHANGED: added login_id; all history queries now filter by login_id
calc_daily_airr <- function(brand_name, login_id, run_date) {
  score_date <- run_date
  brand_id   <- get_or_create_brand_id(con, brand_name)
  
  persistence <- dbGetQuery(con,
                            "SELECT persistence_score FROM fact_persistence_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3;",
                            params = list(brand_id, login_id, score_date))$persistence_score
  
  presence <- dbGetQuery(con,
                         "SELECT overall_score FROM fact_presence_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3;",
                         params = list(brand_id, login_id, score_date))$overall_score
  
  perception <- dbGetQuery(con,
                           "SELECT perception_score FROM fact_perception_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3;",
                           params = list(brand_id, login_id, score_date))$perception_score
  
  prestige <- dbGetQuery(con,
                         "SELECT prestige_score FROM fact_prestige_history
     WHERE brand_id = $1 AND login_id = $2 AND date = $3;",
                         params = list(brand_id, login_id, score_date))$prestige_score
  
  airr_score <- perception  * AIRR_WEIGHTS$perception  +
    presence    * AIRR_WEIGHTS$presence    +
    prestige    * AIRR_WEIGHTS$prestige    +
    persistence * AIRR_WEIGHTS$persistence
  
  dbBegin(con)
  
  dbExecute(con, "
    INSERT INTO fact_airr_history (
      brand_id, login_id, date, airr_score
    ) VALUES ($1, $2, $3, $4)
    ON CONFLICT (brand_id, login_id, date) 
    DO UPDATE SET
      airr_score = EXCLUDED.airr_score;
  ", params = list(brand_id, login_id, score_date, airr_score))
  
  message(sprintf("✓ Inserted airr_score for %s / user %d (brand ID: %d)",
                  brand_name, login_id, brand_id))
  dbCommit(con)
}

# CHANGED: accepts login_id; pulls industry from fact_user_brands_tracked (not dim_brand)
user_create_airr <- function(brand_name, login_id, model = "gpt-4o-mini") {
  
  brand_id <- get_or_create_brand_id(con, brand_name)
  
  # Pull reach/location from dim_brand, but industry from the USER's tracking record
  brand_info <- dbGetQuery(con,
                           "SELECT b.brand_reach, b.reach_country, b.reach_region, b.reach_postcode,
            ubt.industry
     FROM dim_brand b
     JOIN fact_user_brands_tracked ubt
       ON ubt.brand_id = b.brand_id
     WHERE b.brand_id = $1
       AND ubt.login_id = $2
       AND ubt.date_valid_from <= CURRENT_DATE
       AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
     LIMIT 1",
                           params = list(brand_id, login_id))
  
  if (nrow(brand_info) > 0) {
    reach    <- brand_info$brand_reach[1]   %||% "global"
    country  <- brand_info$reach_country[1]
    region   <- brand_info$reach_region[1]
    postcode <- brand_info$reach_postcode[1]
    industry <- brand_info$industry[1]      # <-- from user's tracking record
    if (is.na(country))  country  <- NULL
    if (is.na(region))   region   <- NULL
    if (is.na(postcode)) postcode <- NULL
    if (is.na(industry)) industry <- NULL
  } else {
    reach    <- "global"
    country  <- NULL
    region   <- NULL
    postcode <- NULL
    industry <- NULL
  }
  
  airr_scores <- full_air_score(brand_name, model,
                                industry       = industry,
                                brand_reach    = reach,
                                reach_country  = country,
                                reach_region   = region,
                                reach_postcode = postcode)
  
  # Pass login_id down to all upload functions
  upload_daily_refresh(brand_name, login_id, airr_scores, as.Date(Sys.Date()))
  calc_daily_persistance(brand_name, login_id, Sys.Date())
  calc_daily_airr(brand_name, login_id, Sys.Date())
}

# ============================================
# DAILY REFRESH LOOP
# Only processes brands that are actively tracked by at least one user
# ============================================

# CHANGED: iterates over (login_id, brand_id) pairs — each user's brand gets its own score
daily_refresh_loop <- function(model = "gpt-4o-mini") {
  
  # Get all active (login_id, brand_id) pairs instead of just distinct brands
  user_brand_list <- dbGetQuery(con, "
    SELECT ubt.login_id, b.brand_id, b.brand_name, ubt.industry
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    ORDER BY ubt.login_id, b.brand_name
  ")
  
  message(sprintf("=== Daily Refresh: %d active user-brand pairs ===\n",
                  nrow(user_brand_list)))
  
  failed <- c()
  
  for (i in seq_len(nrow(user_brand_list))) {
    brand     <- user_brand_list$brand_name[i]
    lid       <- user_brand_list$login_id[i]
    
    tryCatch({
      t0 <- Sys.time()
      message(sprintf("\n[%d/%d] Processing %s for user %d...",
                      i, nrow(user_brand_list), brand, lid))
      
      limits <- check_rate_limits(model)
      if (!is.null(limits)) {
        if (!is.null(limits$tokens_remaining) && limits$tokens_remaining < 50000) {
          message(sprintf("  ⏳ Low tokens (%d). Waiting 60s...", limits$tokens_remaining))
          Sys.sleep(60)
        }
        if (!is.null(limits$requests_remaining) && limits$requests_remaining < 100) {
          message(sprintf("  ⏳ Low requests (%d). Waiting 60s...", limits$requests_remaining))
          Sys.sleep(60)
        }
      }
      
      # Pass login_id so scoring uses user's industry
      user_create_airr(brand, lid, model)
      
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      message(sprintf("  ✓ %s / user %d completed in %.1fs", brand, lid, elapsed))
      
    }, error = function(e) {
      failed <<- c(failed, paste0(brand, "/user:", lid))
      message(sprintf("  ✗ %s / user %d failed: %s", brand, lid, e$message))
    })
  }
  
  if (length(failed) > 0) {
    message(sprintf("\n%d/%d user-brand pairs failed: %s",
                    length(failed), nrow(user_brand_list),
                    paste(failed, collapse = ", ")))
  } else {
    message("\nAll user-brand pairs completed successfully!")
  }
  
  # ============================================
  # Score customer profiles (Enterprise only) — unchanged
  # ============================================
  message("\n=== Scoring customer profiles ===")
  
  active_profiles <- dbGetQuery(con, "
    SELECT DISTINCT upt.login_id, upt.profile_id, dcp.profile_name
    FROM fact_user_profiles_tracked upt
    JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
    JOIN fact_user_sub_level fus ON fus.login_id = upt.login_id
    JOIN dim_subscription ds ON ds.subscription_level_id = fus.subscription_level_id
    WHERE ds.subscription_name = 'Enterprise'
      AND upt.date_valid_from <= CURRENT_DATE
      AND (upt.date_valid_to IS NULL OR upt.date_valid_to >= CURRENT_DATE)
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    ORDER BY upt.login_id, upt.profile_id
  ")
  
  if (nrow(active_profiles) == 0) {
    message("No active enterprise profiles to score.")
  } else {
    message(sprintf("Found %d active profiles to score.", nrow(active_profiles)))
    profile_failed <- c()
    
    for (p in seq_len(nrow(active_profiles))) {
      lid   <- active_profiles$login_id[p]
      pid   <- active_profiles$profile_id[p]
      pname <- active_profiles$profile_name[p]
      
      tryCatch({
        t0 <- Sys.time()
        message(sprintf("\n  [%d/%d] Profile: %s (login: %d, profile: %d)...",
                        p, nrow(active_profiles), pname, lid, pid))
        score_profile(lid, pid, model)
        elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        message(sprintf("  ✓ %s completed in %.1fs", pname, elapsed))
      }, error = function(e) {
        profile_failed <<- c(profile_failed, paste0(pname, "(", pid, ")"))
        message(sprintf("  ✗ Profile %s failed: %s", pname, e$message))
      })
    }
    
    if (length(profile_failed) > 0) {
      message(sprintf("\n%d profile(s) failed: %s",
                      length(profile_failed), paste(profile_failed, collapse = ", ")))
    } else {
      message("\nAll profiles scored successfully!")
    }
  }
  
  return(invisible(failed))
}

# ============================================
# DAILY PROMPT LOOP
# Only processes queries actively tracked by at least one user,
# and only for brands linked to users tracking that query
# ============================================

daily_prompt_loop <- function(model = "gpt-4o-mini") {
  
  active_queries <- dbGetQuery(con, "
    SELECT DISTINCT dq.query_id, dq.query_string
    FROM dim_query dq
    INNER JOIN fact_user_queries_tracked uqt ON dq.query_id = uqt.query_id
    WHERE uqt.date_valid_from <= CURRENT_DATE
      AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)
    ORDER BY dq.query_id
  ")
  
  if (nrow(active_queries) == 0) {
    message("No active queries to process.")
    return(invisible(c()))
  }
  
  message(sprintf("=== Daily Prompt Loop: %d active queries ===\n", nrow(active_queries)))
  
  # Initialise failed vector BEFORE the loop
  failed <- c()
  
  for (q in seq_len(nrow(active_queries))) {
    qid     <- active_queries$query_id[q]
    qstring <- active_queries$query_string[q]
    
    tryCatch({
      message(sprintf("\n[%d/%d] Processing query: %s (ID: %d)...",
                      q, nrow(active_queries), substr(qstring, 1, 50), qid))
      
      # Get brands for this query with their reach/location metadata
      brands_for_query <- dbGetQuery(con, "
        SELECT DISTINCT b.brand_id, b.brand_name,
               b.brand_reach, b.reach_country, b.reach_postcode
        FROM fact_user_queries_tracked uqt
        JOIN fact_user_brands_tracked ubt ON uqt.login_id = ubt.login_id
        JOIN dim_brand b ON b.brand_id = ubt.brand_id
        WHERE uqt.query_id = $1
          AND uqt.date_valid_from <= CURRENT_DATE
          AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)
          AND ubt.date_valid_from <= CURRENT_DATE
          AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
        ORDER BY b.brand_id
      ", params = list(qid))
      
      if (nrow(brands_for_query) == 0) {
        message("  No active brands for this query, skipping.")
        next
      }
      
      # Resolve prompt per brand (near_me brands get location injected)
      brands_for_query$query_resolved <- mapply(function(reach, country, postcode) {
        country  <- if (is.na(country))  NULL else country
        postcode <- if (is.na(postcode)) NULL else postcode
        inject_near_me_location(qstring, reach, country, postcode)
      },
      brands_for_query$brand_reach,
      brands_for_query$reach_country,
      brands_for_query$reach_postcode,
      SIMPLIFY = TRUE
      )
      
      # Fetch API responses once per unique resolved prompt
      unique_prompts  <- unique(brands_for_query$query_resolved)
      responses_cache <- list()
      
      for (uq in unique_prompts) {
        query_list <- rep(uq, each = 10)
        responses_cache[[uq]] <- prompt_queries(query_list, model = model)
      }
      
      message(sprintf("  Scoring %d brands (%d unique prompt variants)...",
                      nrow(brands_for_query), length(unique_prompts)))
      
      # Ensure dim_brand_query links exist
      for (b in seq_len(nrow(brands_for_query))) {
        dbExecute(con, "
          INSERT INTO dim_brand_query (brand_id, query_id, date_added)
          VALUES ($1, $2, $3)
          ON CONFLICT (brand_id, query_id) DO NOTHING",
                  params = list(brands_for_query$brand_id[b], qid, Sys.Date()))
      }
      
      for (b in seq_len(nrow(brands_for_query))) {
        bid        <- brands_for_query$brand_id[b]
        brand_name <- brands_for_query$brand_name[b]
        q_resolved <- brands_for_query$query_resolved[b]
        rel_responses <- responses_cache[[q_resolved]]
        
        tryCatch({
          message(sprintf("  [%d/%d] Brand: %s...", b, nrow(brands_for_query), brand_name))
          
          presence_prompt_score   <- presence_prompt_calc(brand_name, rel_responses)
          prestige_prompt_score   <- calculate_prestige_from_prompts_sep(brand_name, bid, login_id, rel_responses)
          perception_prompt_score <- calculate_perception_from_prompts_sep(brand_name, bid, rel_responses)
          
          dbExecute(con, "
            INSERT INTO fact_query_history (
              brand_id, query_id, date, presence_score, perception_score,
              prestige_score, persistence_score, airr_score
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
            ON CONFLICT (brand_id, query_id, date)
            DO UPDATE SET
              presence_score    = EXCLUDED.presence_score,
              perception_score  = EXCLUDED.perception_score,
              prestige_score    = EXCLUDED.prestige_score,
              persistence_score = EXCLUDED.persistence_score,
              airr_score        = EXCLUDED.airr_score;
          ", params = list(
            bid, qid, Sys.Date(),
            presence_prompt_score, perception_prompt_score, prestige_prompt_score, 0, 0
          ))
          
          presence_history <- dbGetQuery(con,
                                         "SELECT date, presence_score FROM fact_query_history
             WHERE brand_id = $1 AND date <= $2 AND query_id = $3
             ORDER BY date",
                                         params = list(bid, Sys.Date(), qid))
          
          if (nrow(presence_history) < 5) {
            persistence_prompt_score <- presence_prompt_score
          } else {
            persistence_prompt_score <- calculate_daily_persistence_sep(presence_history)
          }
          
          todays_scores <- dbGetQuery(con,
                                      "SELECT presence_score, perception_score, prestige_score
             FROM fact_query_history
             WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                                      params = list(bid, Sys.Date(), qid))
          
          airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception +
            todays_scores$presence_score   * AIRR_WEIGHTS$presence +
            todays_scores$prestige_score   * AIRR_WEIGHTS$prestige +
            persistence_prompt_score       * AIRR_WEIGHTS$persistence
          
          dbExecute(con,
                    "UPDATE fact_query_history
             SET persistence_score = $1, airr_score = $2
             WHERE brand_id = $3 AND date = $4 AND query_id = $5",
                    params = list(persistence_prompt_score, airr_score, bid, Sys.Date(), qid))
          
          message(sprintf("    ✓ %s done", brand_name))
          
        }, error = function(e) {
          # failed is initialised above so <<- will find it
          failed <<- c(failed, paste0("brand:", bid, "/query:", qid))
          message(sprintf("    ✗ %s failed: %s", brand_name, e$message))
        })
      }
      
    }, error = function(e) {
      failed <<- c(failed, paste0("query:", qid))
      message(sprintf("  ✗ Query %d failed: %s", qid, e$message))
    })
  }
  
  if (length(failed) > 0) {
    message(sprintf("\n%d failures: %s", length(failed), paste(failed, collapse = ", ")))
  } else {
    message("\nAll queries completed successfully!")
  }
  
  return(invisible(failed))
}

# ============================================
# CREATE PROMPT AIRR - for a single brand + query
# Called from server_account when user adds a query
# ============================================

create_prompt_airr <- function(brand_id,
                               query_string,
                               query_id_in,
                               model = "gpt-4o-mini") {
  
  # Look up brand metadata for reach/location
  brand_meta <- dbGetQuery(con,
                           "SELECT brand_name, brand_reach, reach_country, reach_postcode
     FROM dim_brand WHERE brand_id = $1",
                           params = list(brand_id))
  
  brand_name  <- brand_meta$brand_name[1]
  reach       <- brand_meta$brand_reach[1]  %||% "global"
  nm_country  <- brand_meta$reach_country[1]
  nm_postcode <- brand_meta$reach_postcode[1]
  
  if (!is.null(nm_country)  && is.na(nm_country))  nm_country  <- NULL
  if (!is.null(nm_postcode) && is.na(nm_postcode)) nm_postcode <- NULL
  
  # Inject location into prompt if near_me brand
  query_string_resolved <- inject_near_me_location(
    query_string, reach, nm_country, nm_postcode
  )
  
  query_list    <- rep(query_string_resolved, each = 10)
  rel_responses <- prompt_queries(query_list, model = model)
  
  # Ensure dim_brand_query link exists
  dbExecute(con, "
    INSERT INTO dim_brand_query (brand_id, query_id, date_added)
    VALUES ($1, $2, $3)
    ON CONFLICT (brand_id, query_id) DO NOTHING",
            params = list(brand_id, query_id_in, Sys.Date()))
  
  presence_prompt_score  <- presence_prompt_calc(brand_name, rel_responses)
  prestige_prompt_score   <- calculate_prestige_from_prompts_sep(brand_name, bid, login_id, rel_responses)
  perception_prompt_score <- calculate_perception_from_prompts_sep(brand_name, brand_id, rel_responses)
  
  dbExecute(con, "
    INSERT INTO fact_query_history (
      brand_id, query_id, date, presence_score, perception_score,
      prestige_score, persistence_score, airr_score
    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    ON CONFLICT (brand_id, query_id, date) 
    DO UPDATE SET
      presence_score  = EXCLUDED.presence_score,
      perception_score = EXCLUDED.perception_score,
      prestige_score  = EXCLUDED.prestige_score,
      persistence_score = EXCLUDED.persistence_score,
      airr_score      = EXCLUDED.airr_score;
  ", params = list(
    brand_id, query_id_in, Sys.Date(),
    presence_prompt_score, perception_prompt_score, prestige_prompt_score, 0, 0
  ))
  
  presence_history <- dbGetQuery(con,
                                 "SELECT date, presence_score FROM fact_query_history
     WHERE brand_id = $1 AND date <= $2 AND query_id = $3
     ORDER BY date",
                                 params = list(brand_id, Sys.Date(), query_id_in))
  
  if (nrow(presence_history) < 5) {
    persistence_prompt_score <- presence_prompt_score
  } else {
    persistence_prompt_score <- calculate_daily_persistence_sep(presence_history)
  }
  
  todays_scores <- dbGetQuery(con,
                              "SELECT presence_score, perception_score, prestige_score 
     FROM fact_query_history
     WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                              params = list(brand_id, Sys.Date(), query_id_in))
  
  airr_score <- todays_scores$perception_score  * AIRR_WEIGHTS$perception + 
    todays_scores$presence_score   * AIRR_WEIGHTS$presence +
    todays_scores$prestige_score   * AIRR_WEIGHTS$prestige + 
    persistence_prompt_score       * AIRR_WEIGHTS$persistence
  
  dbExecute(con,
            "UPDATE fact_query_history 
     SET persistence_score = $1, airr_score = $2
     WHERE brand_id = $3 AND date = $4 AND query_id = $5",
            params = list(persistence_prompt_score, airr_score, brand_id, Sys.Date(), query_id_in))
  
  message(sprintf("✓ Prompt AIRR calculated for brand %d, query %d", brand_id, query_id_in))
}

# ============================================
# CREATE PROMPT AIRR MULTIPLE - for multiple brands + one query
# Called from server_account when user adds a query (scores all their brands)
# ============================================

create_prompt_airr_multiple <- function(brand_ids,
                                        query_string,
                                        query_id_in,
                                        model = "gpt-4o-mini") {
  
  message(sprintf("=== create_prompt_airr_multiple: %d brands ===", length(brand_ids)))
  
  # Look up all brand metadata in one query
  placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
  all_brand_meta <- dbGetQuery(con,
                               sprintf("SELECT brand_id, brand_name, brand_reach, reach_country, reach_postcode
             FROM dim_brand WHERE brand_id IN (%s)", placeholders),
                               params = as.list(brand_ids))
  
  # Group brands by their resolved prompt string so we can batch API calls
  # Brands with same resolved prompt can share responses
  all_brand_meta$query_resolved <- mapply(function(reach, country, postcode) {
    country  <- if (is.na(country))  NULL else country
    postcode <- if (is.na(postcode)) NULL else postcode
    inject_near_me_location(query_string, reach, country, postcode)
  },
  all_brand_meta$brand_reach,
  all_brand_meta$reach_country,
  all_brand_meta$reach_postcode
  )
  
  # Get unique resolved prompts and fetch responses once per unique prompt
  unique_prompts <- unique(all_brand_meta$query_resolved)
  
  responses_cache <- list()
  for (uq in unique_prompts) {
    query_list <- rep(uq, each = 10)
    responses_cache[[uq]] <- prompt_queries(query_list, model = model)
    message(sprintf("  Got responses for prompt: %s", substr(uq, 1, 60)))
  }
  
  message(sprintf("Got responses for %d unique prompt variants", length(unique_prompts)))
  
  for (idx in seq_along(brand_ids)) {
    bid <- brand_ids[idx]
    
    meta <- all_brand_meta[all_brand_meta$brand_id == bid, ]
    if (nrow(meta) == 0) {
      warning(sprintf("No metadata found for brand_id %d, skipping", bid))
      next
    }
    
    brand_name        <- meta$brand_name[1]
    query_resolved    <- meta$query_resolved[1]
    rel_responses     <- responses_cache[[query_resolved]]
    
    message(sprintf("  [%d/%d] %s (ID: %d)...", idx, length(brand_ids), brand_name, bid))
    
    # Ensure dim_brand_query link exists
    dbExecute(con, "
      INSERT INTO dim_brand_query (brand_id, query_id, date_added)
      VALUES ($1, $2, $3)
      ON CONFLICT (brand_id, query_id) DO NOTHING",
              params = list(bid, query_id_in, Sys.Date()))
    
    presence_prompt_score   <- presence_prompt_calc(brand_name, rel_responses)
    prestige_prompt_score   <- calculate_prestige_from_prompts_sep(brand_name, bid, login_id, rel_responses)
    perception_prompt_score <- calculate_perception_from_prompts_sep(brand_name, bid, rel_responses)
    
    dbExecute(con, "
      INSERT INTO fact_query_history (
        brand_id, query_id, date, presence_score, perception_score,
        prestige_score, persistence_score, airr_score
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      ON CONFLICT (brand_id, query_id, date) 
      DO UPDATE SET
        presence_score   = EXCLUDED.presence_score,
        perception_score = EXCLUDED.perception_score,
        prestige_score   = EXCLUDED.prestige_score,
        persistence_score = EXCLUDED.persistence_score,
        airr_score       = EXCLUDED.airr_score;
    ", params = list(
      bid, query_id_in, Sys.Date(),
      presence_prompt_score, perception_prompt_score, prestige_prompt_score, 0, 0
    ))
    
    presence_history <- dbGetQuery(con,
                                   "SELECT date, presence_score FROM fact_query_history
       WHERE brand_id = $1 AND date <= $2 AND query_id = $3
       ORDER BY date",
                                   params = list(bid, Sys.Date(), query_id_in))
    
    if (nrow(presence_history) < 5) {
      persistence_prompt_score <- presence_prompt_score
    } else {
      persistence_prompt_score <- calculate_daily_persistence_sep(presence_history)
    }
    
    todays_scores <- dbGetQuery(con,
                                "SELECT presence_score, perception_score, prestige_score
       FROM fact_query_history
       WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                                params = list(bid, Sys.Date(), query_id_in))
    
    airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception +
      todays_scores$presence_score   * AIRR_WEIGHTS$presence +
      todays_scores$prestige_score   * AIRR_WEIGHTS$prestige +
      persistence_prompt_score       * AIRR_WEIGHTS$persistence
    
    dbExecute(con,
              "UPDATE fact_query_history 
       SET persistence_score = $1, airr_score = $2
       WHERE brand_id = $3 AND date = $4 AND query_id = $5",
              params = list(persistence_prompt_score, airr_score, bid, Sys.Date(), query_id_in))
    
    message(sprintf("    ✓ %s done", brand_name))
  }
  
  message("=== create_prompt_airr_multiple completed ===")
}