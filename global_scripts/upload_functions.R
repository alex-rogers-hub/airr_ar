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

# Helper to get or create brand_id
get_or_create_brand_id <- function(con, brand_name) {
  result <- dbGetQuery(con,
                       "SELECT brand_id FROM dim_brand WHERE lower(brand_name) = lower($1);",
                       params = list(brand_name)
  )
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

daily_refresh_loop <- function(model = "gpt-4o-mini") {
  brand_list <- dbGetQuery(con, 'SELECT * FROM dim_brand')
  
  brand_names <- brand_list$brand_name
  
  failed <- c()
  
  for (i in brand_names) {
    tryCatch({
      message(sprintf("Processing %s...", i))
      
      airr_scores <- full_air_score(i, model)
      upload_daily_refresh(i, airr_scores, as.Date(Sys.Date()))
      calc_daily_persistance(i, Sys.Date())
      calc_daily_airr(i, Sys.Date())
      
      message(sprintf("✓ %s completed successfully", i))
      
    }, error = function(e) {
      failed <<- c(failed, i)
      message(sprintf("✗ %s failed: %s", i, e$message))
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

user_create_airr <- function(brand_name, model = "gpt-4o-mini") {
  airr_scores <- full_air_score(brand_name, model)
  upload_daily_refresh(brand_name, airr_scores, as.Date(Sys.Date()))
  calc_daily_persistance(brand_name, Sys.Date())
  calc_daily_airr(brand_name, Sys.Date())
}

daily_prompt_loop <- function(model = "gpt-4o-mini") {
  
  all_queries <- dbGetQuery(con, 'SELECT * FROM dim_query')
  
  queries_unique <- all_queries %>%
    select(query_string) %>%
    unique()
  
  query_list <- rep(queries_unique$query_string, each = 10)
  
  prompt_results <- prompt_queries(query_list, model = model)
  
  pr_with_brand <- prompt_results %>%
    dplyr::rename('query_string' = 'prompt') %>%
    left_join(all_queries, by = 'query_string', relationship = 'many-to-many')
  
  query_ids_unique <- all_queries %>%
    select(query_id) %>%
    unique()
  
  qids <- query_ids_unique$query_id
  
  failed <- c()
  
  for (i in qids) {
    tryCatch({
      message(sprintf("Processing query_id %s...", i))
      
      brands_w_query <- dbGetQuery(con, paste0(
        'SELECT * FROM dim_brand_query WHERE query_id = ', i))
      
      rel_responses <- pr_with_brand %>%
        filter(query_id == i)
      
      if (nrow(brands_w_query) != 0) {
        for (bid in brands_w_query$brand_id) {
          tryCatch({
            brand_name <- dbGetQuery(con, paste0(
              'SELECT brand_name FROM dim_brand WHERE brand_id = ', bid))$brand_name
            
            message(sprintf("  Processing brand %s (%s), query %s...", bid, brand_name, i))
            
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
              bid, i, Sys.Date(),
              presence_prompt_score, perception_prompt_score, prestige_prompt_score, 0, 0
            ))
            
            presence_history <- dbGetQuery(con, paste0(
              "SELECT date, presence_score FROM fact_query_history
               WHERE brand_id = ", bid, 
              " AND date <= '", Sys.Date(), "' AND query_id = ", i, ";"))
            
            if (nrow(presence_history) < 5) {
              persistence_prompt_score <- presence_prompt_score
            } else {
              persistence_prompt_score <- calculate_daily_persistence_sep(presence_history)
            }
            
            todays_scores <- dbGetQuery(con, paste0(
              "SELECT presence_score, perception_score, prestige_score FROM fact_query_history
               WHERE brand_id = ", bid,
              " AND date = '", Sys.Date(), "' AND query_id = ", i, ";"))
            
            airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception + 
              todays_scores$presence_score * AIRR_WEIGHTS$presence +
              todays_scores$prestige_score * AIRR_WEIGHTS$prestige + 
              persistence_prompt_score * AIRR_WEIGHTS$persistence
            
            dbExecute(con, paste0(
              "UPDATE fact_query_history 
               SET persistence_score = ", persistence_prompt_score,
              ", airr_score = ", airr_score,
              " WHERE brand_id = ", bid,
              " AND date = '", Sys.Date(), "' AND query_id = ", i, ";"))
            
            message(sprintf("  ✓ Brand %s (%s), query %s completed", bid, brand_name, i))
            
          }, error = function(e) {
            failed <<- c(failed, paste0("brand:", bid, "/query:", i))
            message(sprintf("  ✗ Brand %s, query %s failed: %s", bid, i, e$message))
          })
        }
      }
    }, error = function(e) {
      failed <<- c(failed, paste0("query:", i))
      message(sprintf("✗ Query %s failed: %s", i, e$message))
    })
  }
  
  if (length(failed) > 0) {
    message(sprintf("\n%d failures: %s", length(failed), paste(failed, collapse = ", ")))
  } else {
    message("\nAll queries completed successfully!")
  }
  
  return(invisible(failed))
}

create_prompt_airr <- function(brand_id,
                               query_string,
                               query_id_in,
                               model = "gpt-4o-mini") {
  
  query_list <- rep(query_string, each = 10)
  prompt_results <- prompt_queries(query_list)
  
  qids <- query_id_in
  
  for (i in qids) {
    brands_w_query <- dbGetQuery(con, paste0(
      'SELECT * FROM dim_brand_query WHERE query_id = ', i))
    
    rel_responses <- prompt_results
    
    if (nrow(brands_w_query) != 0) {
      brand_name <- dbGetQuery(con, paste0(
        'SELECT brand_name FROM dim_brand WHERE brand_id = ', brands_w_query$brand_id))$brand_name
      
      bid <- brands_w_query$brand_id
      
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
        bid, i, Sys.Date(),
        presence_prompt_score, perception_prompt_score, prestige_prompt_score, 0, 0
      ))
      
      presence_history <- dbGetQuery(con, paste0(
        "SELECT date, presence_score FROM fact_query_history
         WHERE brand_id = ", bid, 
        " AND date <= '", Sys.Date(), "' AND query_id = ", i, ";"))
      
      if (nrow(presence_history) < 5) {
        persistence_prompt_score <- 100
      } else {
        persistence_prompt_score <- calculate_daily_persistence_sep(presence_history)
      }
      
      todays_scores <- dbGetQuery(con, paste0(
        "SELECT presence_score, perception_score, prestige_score FROM fact_query_history
         WHERE brand_id = ", bid,
        " AND date = '", Sys.Date(), "' AND query_id = ", i, ";"))
      
      airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception + 
        todays_scores$presence_score * AIRR_WEIGHTS$presence +
        todays_scores$prestige_score * AIRR_WEIGHTS$prestige + 
        persistence_prompt_score * AIRR_WEIGHTS$persistence
      
      dbExecute(con, paste0(
        "UPDATE fact_query_history 
         SET persistence_score = ", persistence_prompt_score,
        ", airr_score = ", airr_score,
        " WHERE brand_id = ", bid,
        " AND date <= '", Sys.Date(), "' AND query_id = ", i, ";"))
    }
  }
}

create_prompt_airr_multiple <- function(brand_ids,
                                        query_string,
                                        query_id_in,
                                        model = "gpt-4o-mini") {
  
  cat("=== Starting create_prompt_airr_multiple ===\n")
  cat("Processing", length(brand_ids), "brands\n")
  
  cat("Step A: Calling prompt_queries\n")
  query_list <- rep(query_string, each = 10)
  rel_responses <- prompt_queries(query_list)
  cat("Got", nrow(rel_responses), "responses\n")
  
  for (idx in seq_along(brand_ids)) {
    bid <- brand_ids[idx]
    cat("Processing brand", idx, "of", length(brand_ids), "(ID:", bid, ")\n")
    
    cat("  Getting brand name...\n")
    brand_name <- dbGetQuery(con, paste0(
      'SELECT brand_name FROM dim_brand WHERE brand_id = ', bid))$brand_name
    cat("  Brand:", brand_name, "\n")
    
    cat("  Calculating presence...\n")
    presence_prompt_score <- presence_prompt_calc(brand_name, rel_responses)
    
    cat("  Calculating prestige...\n")
    prestige_prompt_score <- calculate_prestige_from_prompts_sep(brand_name, bid, rel_responses)
    
    cat("  Calculating perception...\n")
    perception_prompt_score <- calculate_perception_from_prompts_sep(brand_name, bid, rel_responses)
    
    cat("  Inserting scores...\n")
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
    
    cat("  Getting presence history...\n")
    presence_history <- dbGetQuery(con, paste0(
      "SELECT date, presence_score FROM fact_query_history
       WHERE brand_id = ", bid, 
      " AND date <= '", Sys.Date(), "' AND query_id = ", query_id_in, ";"))
    
    if (nrow(presence_history) < 5) {
      persistence_prompt_score <- 100
    } else {
      cat("  Calculating persistence...\n")
      persistence_prompt_score <- calculate_daily_persistence_sep(presence_history)
    }
    
    cat("  Getting today's scores...\n")
    todays_scores <- dbGetQuery(con, paste0(
      "SELECT presence_score, perception_score, prestige_score FROM fact_query_history
       WHERE brand_id = ", bid,
      " AND date = '", Sys.Date(), "' AND query_id = ", query_id_in, ";"))
    
    cat("  Calculating AIRR score...\n")
    airr_score <- todays_scores$perception_score * AIRR_WEIGHTS$perception + 
      todays_scores$presence_score * AIRR_WEIGHTS$presence +
      todays_scores$prestige_score * AIRR_WEIGHTS$prestige + 
      persistence_prompt_score * AIRR_WEIGHTS$persistence
    
    cat("  Updating with final scores...\n")
    dbExecute(con, paste0(
      "UPDATE fact_query_history 
       SET persistence_score = ", persistence_prompt_score,
      ", airr_score = ", airr_score,
      " WHERE brand_id = ", bid,
      " AND date <= '", Sys.Date(), "' AND query_id = ", query_id_in, ";"))
    
    cat("  Completed brand", bid, "\n")
  }
  
  cat("=== create_prompt_airr_multiple completed ===\n")
}