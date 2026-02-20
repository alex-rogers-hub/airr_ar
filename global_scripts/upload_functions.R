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

upload_daily_refresh <- function(brand_name, input_data, run_date) {
  
  dbBegin(con)
  
  tryCatch({
    score_date <- run_date
    air_scores <- input_data
    brand_id <- get_or_create_brand_id(con, brand_name)
    
    # ==================== INSERT PRESENCE DATA ====================
    presence <- air_scores$presence
    
    dbExecute(con, "
      INSERT INTO fact_presence_history (
        brand_id, date, overall_score, simple_mention_rate,
        total_responses, responses_with_mentions, interpretation, stability
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      ON CONFLICT (brand_id, date) 
      DO UPDATE SET
        overall_score = EXCLUDED.overall_score,
        simple_mention_rate = EXCLUDED.simple_mention_rate,
        total_responses = EXCLUDED.total_responses,
        responses_with_mentions = EXCLUDED.responses_with_mentions,
        interpretation = EXCLUDED.interpretation,
        stability = EXCLUDED.stability;
    ", params = list(
      brand_id, score_date,
      presence$overall_score, presence$simple_mention_rate,
      presence$total_responses, presence$responses_with_mentions,
      presence$interpretation, presence$stability
    ))
    
    message(sprintf("✓ Inserted presence data for %s (ID: %d)", brand_name, brand_id))
    
    # ==================== INSERT PERCEPTION DATA ====================
    perception <- air_scores$perception
    
    dbExecute(con, "
      INSERT INTO fact_perception_history (
        brand_id, date, perception_score, perception_accuracy_score,
        perception_sentiment_score, prestige_accuracy_interpretation
      ) VALUES ($1, $2, $3, $4, $5, $6)
      ON CONFLICT (brand_id, date) 
      DO UPDATE SET
        perception_score = EXCLUDED.perception_score,
        perception_accuracy_score = EXCLUDED.perception_accuracy_score,
        perception_sentiment_score = EXCLUDED.perception_sentiment_score,
        prestige_accuracy_interpretation = EXCLUDED.prestige_accuracy_interpretation;
    ", params = list(
      brand_id, score_date,
      perception$perception_score, perception$perception_accuracy_score,
      perception$perception_sentiment_score, perception$prestige_accuracy_interpretation
    ))
    
    message(sprintf("✓ Inserted perception data for %s (ID: %d)", brand_name, brand_id))
    
    # ==================== INSERT PRESTIGE DATA ====================
    prestige <- air_scores$prestige
    
    dbExecute(con, "
      INSERT INTO fact_prestige_history (
        brand_id, date, prestige_score, prestige_rank_score,
        prestige_rank_comps_brands, prestige_authority_score, prestige_leadership_score
      ) VALUES ($1, $2, $3, $4, $5, $6, $7)
      ON CONFLICT (brand_id, date) 
      DO UPDATE SET
        prestige_score = EXCLUDED.prestige_score,
        prestige_rank_score = EXCLUDED.prestige_rank_score,
        prestige_rank_comps_brands = EXCLUDED.prestige_rank_comps_brands,
        prestige_authority_score = EXCLUDED.prestige_authority_score,
        prestige_leadership_score = EXCLUDED.prestige_leadership_score;
    ", params = list(
      brand_id, score_date,
      prestige$prestige_score, prestige$prestige_rank_score,
      paste(prestige$prestige_rank_comps$brand, collapse = " | "),
      prestige$prestige_authority_score, prestige$prestige_leadership_score
    ))
    message(sprintf("✓ Inserted prestige data for %s (ID: %d)", brand_name, brand_id))
    
    dbCommit(con)
    message(sprintf("✓✓ Successfully inserted all AIR scores for %s", brand_name))
    return(TRUE)
    
  }, error = function(e) {
    dbRollback(con)
    warning(sprintf("✗ Error inserting data for %s: %s", brand_name, e$message))
    return(FALSE)
  })
}

calc_daily_persistance <- function(brand_name, run_date) {
  score_date <- run_date
  persistence <- calculate_daily_persistence(brand_name, score_date)
  brand_id <- get_or_create_brand_id(con, brand_name)
  
  dbBegin(con)
  
  dbExecute(con, "
    INSERT INTO fact_persistence_history (
      brand_id, date, persistence_score, coefficient_of_variation,
      interpretation, daily_perc_change
    ) VALUES ($1, $2, $3, $4, $5, $6)
    ON CONFLICT (brand_id, date) 
    DO UPDATE SET
      persistence_score = EXCLUDED.persistence_score,
      coefficient_of_variation = EXCLUDED.coefficient_of_variation,
      interpretation = EXCLUDED.interpretation,
      daily_perc_change = EXCLUDED.daily_perc_change;
  ", params = list(
    brand_id, score_date,
    persistence$score, persistence$coefficient_of_variation,
    persistence$interpretation, persistence$daily_perc_change
  ))
  message(sprintf("✓ Inserted persistence data for %s (ID: %d)", brand_name, brand_id))
  dbCommit(con)
}

calc_daily_airr <- function(brand_name, run_date) {
  score_date <- run_date
  brand_id <- get_or_create_brand_id(con, brand_name)
  
  persistence <- dbGetQuery(con,
                            "SELECT persistence_score FROM fact_persistence_history
     WHERE brand_id = $1 AND date = $2;",
                            params = list(brand_id, score_date))$persistence_score
  
  presence <- dbGetQuery(con,
                         "SELECT overall_score FROM fact_presence_history
     WHERE brand_id = $1 AND date = $2;",
                         params = list(brand_id, score_date))$overall_score
  
  perception <- dbGetQuery(con,
                           "SELECT perception_score FROM fact_perception_history
     WHERE brand_id = $1 AND date = $2;",
                           params = list(brand_id, score_date))$perception_score
  
  prestige <- dbGetQuery(con,
                         "SELECT prestige_score FROM fact_prestige_history
     WHERE brand_id = $1 AND date = $2;",
                         params = list(brand_id, score_date))$prestige_score
  
  airr_score <- perception * AIRR_WEIGHTS$perception + 
    presence * AIRR_WEIGHTS$presence +
    prestige * AIRR_WEIGHTS$prestige + 
    persistence * AIRR_WEIGHTS$persistence
  
  dbBegin(con)
  
  dbExecute(con, "
    INSERT INTO fact_airr_history (
      brand_id, date, airr_score
    ) VALUES ($1, $2, $3)
    ON CONFLICT (brand_id, date) 
    DO UPDATE SET
      airr_score = EXCLUDED.airr_score;
  ", params = list(brand_id, score_date, airr_score))
  
  message(sprintf("✓ Inserted airr_score data for %s (ID: %d)", brand_name, brand_id))
  dbCommit(con)
}

user_create_airr <- function(brand_name, model = "gpt-4o-mini") {
  airr_scores <- full_air_score(brand_name, model)
  upload_daily_refresh(brand_name, airr_scores, as.Date(Sys.Date()))
  calc_daily_persistance(brand_name, Sys.Date())
  calc_daily_airr(brand_name, Sys.Date())
}

# ============================================
# DAILY REFRESH LOOP
# Only processes brands that are actively tracked by at least one user
# ============================================

daily_refresh_loop <- function(model = "gpt-4o-mini") {
  
  # Get only brands that are actively tracked by at least one user
  brand_list <- dbGetQuery(con, "
    SELECT DISTINCT b.brand_id, b.brand_name
    FROM dim_brand b
    INNER JOIN fact_user_brands_tracked ubt ON b.brand_id = ubt.brand_id
    WHERE ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    ORDER BY b.brand_name
  ")
  
  brand_names <- brand_list$brand_name
  
  message(sprintf("=== Daily Refresh: %d active brands ===\n", length(brand_names)))
  
  failed <- c()
  
  for (i in seq_along(brand_names)) {
    brand <- brand_names[i]
    
    tryCatch({
      t0 <- Sys.time()
      message(sprintf("\n[%d/%d] Processing %s...", i, length(brand_names), brand))
      
      # Check rate limits before each brand
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
      
      airr_scores <- full_air_score(brand, model)
      upload_daily_refresh(brand, airr_scores, as.Date(Sys.Date()))
      calc_daily_persistance(brand, Sys.Date())
      calc_daily_airr(brand, Sys.Date())
      
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      message(sprintf("  ✓ %s completed in %.1fs", brand, elapsed))
      
    }, error = function(e) {
      failed <<- c(failed, brand)
      message(sprintf("  ✗ %s failed: %s", brand, e$message))
    })
  }
  
  if (length(failed) > 0) {
    message(sprintf("\n%d/%d brands failed: %s", 
                    length(failed), length(brand_names), 
                    paste(failed, collapse = ", ")))
  } else {
    message("\nAll brands completed successfully!")
  }
  
  return(invisible(failed))
}

# ============================================
# DAILY PROMPT LOOP
# Only processes queries actively tracked by at least one user,
# and only for brands linked to users tracking that query
# ============================================

daily_prompt_loop <- function(model = "gpt-4o-mini") {
  
  # Get only queries actively tracked by at least one user
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
  
  # Send all unique queries to API in one batch
  query_list <- rep(active_queries$query_string, each = 10)
  
  t_api <- Sys.time()
  prompt_results <- prompt_queries(query_list, model = model)
  message(sprintf("⏱ API calls: %.1fs for %d prompts", 
                  as.numeric(difftime(Sys.time(), t_api, units = "secs")),
                  length(query_list)))
  
  pr_with_query <- prompt_results %>%
    dplyr::rename('query_string' = 'prompt') %>%
    left_join(active_queries, by = 'query_string', relationship = 'many-to-many')
  
  failed <- c()
  
  for (q in seq_len(nrow(active_queries))) {
    qid <- active_queries$query_id[q]
    qstring <- active_queries$query_string[q]
    
    tryCatch({
      message(sprintf("\n[%d/%d] Processing query: %s (ID: %d)...", 
                      q, nrow(active_queries), substr(qstring, 1, 50), qid))
      
      rel_responses <- pr_with_query %>% filter(query_id == qid)
      
      # Get brands that need scoring for this query:
      # All brands linked to users who track this query
      brands_for_query <- dbGetQuery(con, "
        SELECT DISTINCT b.brand_id, b.brand_name
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
      
      message(sprintf("  Scoring %d brands...", nrow(brands_for_query)))
      
      # Ensure dim_brand_query links exist
      for (b in seq_len(nrow(brands_for_query))) {
        dbExecute(con, "
          INSERT INTO dim_brand_query (brand_id, query_id, date_added)
          VALUES ($1, $2, $3)
          ON CONFLICT (brand_id, query_id) DO NOTHING",
                  params = list(brands_for_query$brand_id[b], qid, Sys.Date()))
      }
      
      for (b in seq_len(nrow(brands_for_query))) {
        bid <- brands_for_query$brand_id[b]
        brand_name <- brands_for_query$brand_name[b]
        
        tryCatch({
          message(sprintf("  [%d/%d] Brand: %s...", b, nrow(brands_for_query), brand_name))
          
          presence_prompt_score <- presence_prompt_calc(brand_name, rel_responses)
          prestige_prompt_score <- calculate_prestige_from_prompts_sep(brand_name, bid, rel_responses)
          perception_prompt_score <- calculate_perception_from_prompts_sep(brand_name, bid, rel_responses)
          
          dbExecute(con, "
            INSERT INTO fact_query_history (
              brand_id, query_id, date, presence_score, perception_score,
              prestige_score, persistence_score, airr_score
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
            ON CONFLICT (brand_id, query_id, date) 
            DO UPDATE SET
              presence_score = EXCLUDED.presence_score,
              perception_score = EXCLUDED.perception_score,
              prestige_score = EXCLUDED.prestige_score,
              persistence_score = EXCLUDED.persistence_score,
              airr_score = EXCLUDED.airr_score;
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
            todays_scores$presence_score * AIRR_WEIGHTS$presence +
            todays_scores$prestige_score * AIRR_WEIGHTS$prestige + 
            persistence_prompt_score * AIRR_WEIGHTS$persistence
          
          dbExecute(con,
                    "UPDATE fact_query_history 
             SET persistence_score = $1, airr_score = $2
             WHERE brand_id = $3 AND date = $4 AND query_id = $5",
                    params = list(persistence_prompt_score, airr_score, bid, Sys.Date(), qid))
          
          message(sprintf("    ✓ %s done", brand_name))
          
        }, error = function(e) {
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
  
  query_list <- rep(query_string, each = 10)
  rel_responses <- prompt_queries(query_list, model = model)
  
  brand_name <- dbGetQuery(con,
                           "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                           params = list(brand_id))$brand_name
  
  # Ensure dim_brand_query link exists
  dbExecute(con, "
    INSERT INTO dim_brand_query (brand_id, query_id, date_added)
    VALUES ($1, $2, $3)
    ON CONFLICT (brand_id, query_id) DO NOTHING",
            params = list(brand_id, query_id_in, Sys.Date()))
  
  presence_prompt_score <- presence_prompt_calc(brand_name, rel_responses)
  prestige_prompt_score <- calculate_prestige_from_prompts_sep(brand_name, brand_id, rel_responses)
  perception_prompt_score <- calculate_perception_from_prompts_sep(brand_name, brand_id, rel_responses)
  
  dbExecute(con, "
    INSERT INTO fact_query_history (
      brand_id, query_id, date, presence_score, perception_score,
      prestige_score, persistence_score, airr_score
    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    ON CONFLICT (brand_id, query_id, date) 
    DO UPDATE SET
      presence_score = EXCLUDED.presence_score,
      perception_score = EXCLUDED.perception_score,
      prestige_score = EXCLUDED.prestige_score,
      persistence_score = EXCLUDED.persistence_score,
      airr_score = EXCLUDED.airr_score;
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
                              "SELECT presence_score, perception_score, prestige_score FROM fact_query_history
     WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                              params = list(brand_id, Sys.Date(), query_id_in))
  
  airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception + 
    todays_scores$presence_score * AIRR_WEIGHTS$presence +
    todays_scores$prestige_score * AIRR_WEIGHTS$prestige + 
    persistence_prompt_score * AIRR_WEIGHTS$persistence
  
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
  
  query_list <- rep(query_string, each = 10)
  rel_responses <- prompt_queries(query_list, model = model)
  message(sprintf("Got %d responses", nrow(rel_responses)))
  
  for (idx in seq_along(brand_ids)) {
    bid <- brand_ids[idx]
    
    brand_name <- dbGetQuery(con,
                             "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                             params = list(bid))$brand_name
    
    message(sprintf("  [%d/%d] %s (ID: %d)...", idx, length(brand_ids), brand_name, bid))
    
    # Ensure dim_brand_query link exists
    dbExecute(con, "
      INSERT INTO dim_brand_query (brand_id, query_id, date_added)
      VALUES ($1, $2, $3)
      ON CONFLICT (brand_id, query_id) DO NOTHING",
              params = list(bid, query_id_in, Sys.Date()))
    
    presence_prompt_score <- presence_prompt_calc(brand_name, rel_responses)
    prestige_prompt_score <- calculate_prestige_from_prompts_sep(brand_name, bid, rel_responses)
    perception_prompt_score <- calculate_perception_from_prompts_sep(brand_name, bid, rel_responses)
    
    dbExecute(con, "
      INSERT INTO fact_query_history (
        brand_id, query_id, date, presence_score, perception_score,
        prestige_score, persistence_score, airr_score
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      ON CONFLICT (brand_id, query_id, date) 
      DO UPDATE SET
        presence_score = EXCLUDED.presence_score,
        perception_score = EXCLUDED.perception_score,
        prestige_score = EXCLUDED.prestige_score,
        persistence_score = EXCLUDED.persistence_score,
        airr_score = EXCLUDED.airr_score;
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
                                "SELECT presence_score, perception_score, prestige_score FROM fact_query_history
       WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                                params = list(bid, Sys.Date(), query_id_in))
    
    airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception + 
      todays_scores$presence_score * AIRR_WEIGHTS$presence +
      todays_scores$prestige_score * AIRR_WEIGHTS$prestige + 
      persistence_prompt_score * AIRR_WEIGHTS$persistence
    
    dbExecute(con,
              "UPDATE fact_query_history 
       SET persistence_score = $1, airr_score = $2
       WHERE brand_id = $3 AND date = $4 AND query_id = $5",
              params = list(persistence_prompt_score, airr_score, bid, Sys.Date(), query_id_in))
    
    message(sprintf("    ✓ %s done", brand_name))
  }
  
  message("=== create_prompt_airr_multiple completed ===")
}