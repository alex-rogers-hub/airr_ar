# Helper function to fetch brand dashboard data
get_brand_metrics <- function(brand_id) {
  queryDC <- "SELECT brand_id, brand_name
              FROM dim_brand
              WHERE brand_id = $1"
  queryA <- "SELECT * FROM fact_presence_history
             WHERE brand_id = $1"
  queryB <- "SELECT * FROM fact_perception_history
             WHERE brand_id = $1"
  queryC <- "SELECT * FROM fact_prestige_history
             WHERE brand_id = $1"
  queryD <- "SELECT * FROM fact_persistence_history
             WHERE brand_id = $1"
  queryE <- "SELECT * FROM fact_airr_history
             WHERE brand_id = $1"
  
  brand_metrics <- list(
    brand = dbGetQuery(pool, queryDC, params = list(brand_id)),
    presence = dbGetQuery(pool, queryA, params = list(brand_id)),
    perception = dbGetQuery(pool, queryB, params = list(brand_id)),
    prestige = dbGetQuery(pool, queryC, params = list(brand_id)),
    persistence = dbGetQuery(pool, queryD, params = list(brand_id)),
    airr = dbGetQuery(pool, queryE, params = list(brand_id))
  )
  return(brand_metrics)
}

# Function to get latest score with specific column name
get_latest_score <- function(df, score_col) {
  if (nrow(df) == 0) return(0)
  latest_idx <- which.max(df$date)
  return(as.numeric(df[[score_col]][latest_idx]))
}

addPrompt <- function(prompt_string, brand_id) {
  con <- poolCheckout(pool)
  
  tryCatch({
    # Insert query and get ID
    query <- "INSERT INTO dim_query (query_string) 
              VALUES ($1) 
              RETURNING query_id"
    
    result <- dbGetQuery(con, query, params = list(prompt_string))
    
    # Insert into junction table
    dbExecute(con, "
      INSERT INTO dim_brand_query (
        brand_id,
        query_id,
        date_added
      ) VALUES ($1, $2, $3)
      ON CONFLICT (brand_id, query_id)
      DO UPDATE SET
        date_added = EXCLUDED.date_added
    ", params = list(
      brand_id,
      result$query_id,
      Sys.Date()
    ))
    
    # Calculate the score for this prompt
    create_prompt_airr(brand_id,
                       prompt_string,
                       result$query_id)
    
    return(result$query_id)
    
  }, error = function(e) {
    warning(paste("Error in addPrompt:", e$message))
    return(NULL)
    
  }, finally = {
    poolReturn(con)
  })
}

get_brand_queries <- function(brand_id) {
  query <- "SELECT dbq.brand_id, dbq.query_id, dbq.date_added,
                   dq.query_string
            FROM dim_brand_query dbq
            JOIN dim_query dq ON dbq.query_id = dq.query_id
            WHERE dbq.brand_id = $1
            ORDER BY dbq.date_added DESC"
  
  dbGetQuery(pool, query, params = list(brand_id))
}

# Helper function to get query history
get_query_history <- function(query_id) {
  query <- "SELECT query_id, date, airr_score, 
                   presence_score, perception_score, 
                   prestige_score, persistence_score
            FROM fact_query_history
            WHERE query_id = $1
            ORDER BY date"
  
  dbGetQuery(pool, query, params = list(query_id))
}

# Helper function to get latest query scores
get_latest_query_scores <- function(query_id, brand_id) {
  query <- "SELECT query_id, date, airr_score, 
                   presence_score, perception_score, 
                   prestige_score, persistence_score
            FROM fact_query_history
            WHERE query_id = $1
            AND brand_id = $2
            ORDER BY date DESC
            LIMIT 1"
  
  result <- dbGetQuery(pool, query, params = list(query_id, brand_id))
  
  if (nrow(result) == 0) {
    return(list(
      airr_score = 0,
      presence_score = 0,
      perception_score = 0,
      prestige_score = 0,
      persistence_score = 0
    ))
  }
  
  return(as.list(result[1, ]))
}

addPromptForEveryone <- function(prompt_string) {
  cat("=== Starting addPromptForEveryone ===\n")
  
  tryCatch({
    
    cat("Step 1: Getting brands\n")
    all_brands <- dbGetQuery(
      pool,
      "SELECT DISTINCT brand_id FROM dim_brand"
    )$brand_id
    cat("Found", length(all_brands), "brands\n")
    
    cat("Step 2: Starting transaction\n")
    con_tx <- poolCheckout(pool)
    dbBegin(con_tx)
    
    cat("Step 3: Inserting query\n")
    query <- "INSERT INTO dim_query (query_string) 
              VALUES ($1) 
              RETURNING query_id"
    
    result <- dbGetQuery(con_tx, query, params = list(prompt_string))
    query_id <- result$query_id
    cat("Query inserted with ID:", query_id, "\n")
    
    cat("Step 4: Building junction table values\n")
    current_date <- Sys.Date()
    values_strings <- paste0(
      "(",
      all_brands,
      ", ",
      query_id,
      ", '",
      current_date,
      "')"
    )
    
    cat("Step 5: Inserting into junction table\n")
    query <- paste0("
      INSERT INTO dim_brand_query (
        brand_id,
        query_id,
        date_added
      )
      VALUES ",
                    paste(values_strings, collapse = ", "),
                    " ON CONFLICT (brand_id, query_id)
      DO UPDATE SET
        date_added = EXCLUDED.date_added
    ")
    
    dbExecute(con_tx, query)
    cat("Junction table updated\n")
    
    cat("Step 6: Committing transaction\n")
    dbCommit(con_tx)
    poolReturn(con_tx)
    
    cat("Step 7: About to call create_prompt_airr_multiple\n")
    create_prompt_airr_multiple(all_brands, prompt_string, query_id)
    cat("Step 8: Finished create_prompt_airr_multiple\n")
    
    cat("=== addPromptForEveryone completed successfully ===\n")
    return(query_id)
    
  }, error = function(e) {
    cat("=== ERROR in addPromptForEveryone ===\n")
    cat("Error message:", e$message, "\n")
    
    tryCatch({
      dbRollback(con_tx)
      poolReturn(con_tx)
    }, error = function(e2) {
      cat("Could not rollback:", e2$message, "\n")
    })
    
    stop(e)
  })
}

get_user_brands <- function(login_id) {
  query <- "SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
            FROM fact_user_brands_tracked ubt
            JOIN dim_brand b ON b.brand_id = ubt.brand_id
            WHERE ubt.login_id = $1
              AND ubt.date_valid_from <= CURRENT_DATE
              AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)"
  
  dbGetQuery(pool, query, params = list(login_id))
}

add_brand_for_user <- function(login_id, brand_name, main_brand = FALSE) {
  result <- add_brand_for_user_pending(login_id, brand_name, main_brand)
  if (!is.null(result)) {
    return(result$brand_id)
  }
  return(NULL)
}


# ============================================
# Brand management helpers
# ============================================

#' Get user's subscription details
get_user_subscription <- function(login_id) {
  query <- "
    SELECT fus.*, ds.subscription_name, ds.num_competitors_included
    FROM fact_user_sub_level fus
    JOIN dim_subscription ds ON fus.subscription_level_id = ds.subscription_level_id
    WHERE fus.login_id = $1
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    ORDER BY ds.num_competitors_included DESC
    LIMIT 1
  "
  result <- dbGetQuery(pool, query, params = list(login_id))
  
  if (nrow(result) == 0) {
    # Default free tier: 3 competitor brands
    return(list(
      subscription_name = "Free",
      num_competitors_included = 3,
      extra_competitors_added = 0,
      max_brands = 3 + 1  # 3 competitors + 1 main brand
    ))
  }
  
  return(list(
    subscription_name = result$subscription_name,
    num_competitors_included = result$num_competitors_included,
    extra_competitors_added = result$extra_competitors_added,
    max_brands = result$num_competitors_included + result$extra_competitors_added + 1  # +1 for main brand
  ))
}

#' Get count of user's active brands (excluding main)
get_user_competitor_count <- function(login_id) {
  query <- "
    SELECT COUNT(*) as cnt
    FROM fact_user_brands_tracked
    WHERE login_id = $1
      AND main_brand_flag = FALSE
      AND date_valid_from <= CURRENT_DATE
      AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
  "
  dbGetQuery(pool, query, params = list(login_id))$cnt
}

#' Remove a competitor brand for a user (soft delete by setting date_valid_to)
remove_brand_for_user <- function(login_id, brand_id) {
  tryCatch({
    dbExecute(pool, "
      UPDATE fact_user_brands_tracked
      SET date_valid_to = CURRENT_DATE
      WHERE login_id = $1 
        AND brand_id = $2 
        AND main_brand_flag = FALSE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
    ", params = list(login_id, brand_id))
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error removing brand:", e$message))
    return(FALSE)
  })
}

#' Get user's main brand
get_user_main_brand <- function(login_id) {
  query <- "
    SELECT b.brand_id, b.brand_name
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LIMIT 1
  "
  dbGetQuery(pool, query, params = list(login_id))
}

#' Get user's competitor brands (non-main)
get_user_competitor_brands <- function(login_id) {
  query <- "
    SELECT b.brand_id, b.brand_name, ubt.date_valid_from
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.main_brand_flag = FALSE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    ORDER BY ubt.date_valid_from DESC
  "
  dbGetQuery(pool, query, params = list(login_id))
}

#' Add a brand for user and mark as pending (no AIRR calculation yet)
add_brand_for_user_pending <- function(login_id, brand_name, main_brand = FALSE) {
  tryCatch({
    # Check if brand already exists
    existing <- dbGetQuery(pool, 
                           "SELECT brand_id FROM dim_brand WHERE lower(brand_name) = lower($1)", 
                           params = list(brand_name))
    
    brand_is_new <- (nrow(existing) == 0)
    
    if (brand_is_new) {
      existing <- dbGetQuery(pool,
                             "INSERT INTO dim_brand (brand_name) VALUES ($1) RETURNING brand_id",
                             params = list(brand_name))
    }
    
    brand_id <- existing$brand_id[1]
    
    # Link user to brand
    dbExecute(pool, "
      INSERT INTO fact_user_brands_tracked (login_id, brand_id, main_brand_flag, date_valid_from)
      VALUES ($1, $2, $3, $4)
      ON CONFLICT (login_id, brand_id, date_valid_from) DO NOTHING",
              params = list(login_id, brand_id, main_brand, Sys.Date()))
    
    # Check if brand already has scores
    has_scores <- brand_has_scores(brand_id)
    
    return(list(
      brand_id = brand_id,
      needs_scoring = !has_scores
    ))
  }, error = function(e) {
    warning(paste("Error in add_brand_for_user_pending:", e$message))
    return(NULL)
  })
}

#' Check if a brand has AIRR scores calculated
brand_has_scores <- function(brand_id) {
  result <- dbGetQuery(pool, "
    SELECT COUNT(*) as cnt 
    FROM fact_airr_history 
    WHERE brand_id = $1",
                       params = list(brand_id))
  return(result$cnt > 0)
}

#' Get user's competitor brands with score status
get_user_competitor_brands_with_status <- function(login_id) {
  query <- "
    SELECT b.brand_id, b.brand_name, ubt.date_valid_from,
      CASE WHEN EXISTS (
        SELECT 1 FROM fact_airr_history fa WHERE fa.brand_id = b.brand_id
      ) THEN TRUE ELSE FALSE END as has_scores
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.main_brand_flag = FALSE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    ORDER BY ubt.date_valid_from DESC
  "
  dbGetQuery(pool, query, params = list(login_id))
}



# ============================================
# Query tracking helpers
# ============================================

#' Get user's subscription details (updated with prompt info)
get_user_subscription <- function(login_id) {
  query <- "
    SELECT fus.*, ds.subscription_name, ds.num_competitors_included, ds.num_prompts_included
    FROM fact_user_sub_level fus
    JOIN dim_subscription ds ON fus.subscription_level_id = ds.subscription_level_id
    WHERE fus.login_id = $1
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    ORDER BY ds.num_competitors_included DESC
    LIMIT 1
  "
  result <- dbGetQuery(pool, query, params = list(login_id))
  
  if (nrow(result) == 0) {
    return(list(
      subscription_name = "Free",
      num_competitors_included = 1,
      num_prompts_included = 1,
      extra_competitors_added = 0,
      extra_prompts_added = 0,
      max_brands = 1 + 1,
      max_prompts = 1
    ))
  }
  
  return(list(
    subscription_name = result$subscription_name,
    num_competitors_included = result$num_competitors_included,
    num_prompts_included = result$num_prompts_included,
    extra_competitors_added = result$extra_competitors_added,
    extra_prompts_added = result$extra_prompts_added,
    max_brands = result$num_competitors_included + result$extra_competitors_added + 1,
    max_prompts = result$num_prompts_included + result$extra_prompts_added
  ))
}

#' Get count of user's active tracked queries
get_user_query_count <- function(login_id) {
  query <- "
    SELECT COUNT(*) as cnt
    FROM fact_user_queries_tracked
    WHERE login_id = $1
      AND date_valid_from <= CURRENT_DATE
      AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
  "
  dbGetQuery(pool, query, params = list(login_id))$cnt
}

#' Get user's active tracked queries with details
get_user_tracked_queries <- function(login_id) {
  query <- "
    SELECT dq.query_id, dq.query_string, uqt.date_valid_from
    FROM fact_user_queries_tracked uqt
    JOIN dim_query dq ON dq.query_id = uqt.query_id
    WHERE uqt.login_id = $1
      AND uqt.date_valid_from <= CURRENT_DATE
      AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)
    ORDER BY uqt.date_valid_from DESC
  "
  dbGetQuery(pool, query, params = list(login_id))
}

#' Add a query for a user to track
#' Creates the query if new, links to user, and links to all user's brands
add_query_for_user <- function(login_id, query_string) {
  tryCatch({
    # Check if query already exists
    existing_query <- dbGetQuery(pool,
                                 "SELECT query_id FROM dim_query WHERE query_string = $1",
                                 params = list(query_string))
    
    if (nrow(existing_query) == 0) {
      existing_query <- dbGetQuery(pool,
                                   "INSERT INTO dim_query (query_string) VALUES ($1) RETURNING query_id",
                                   params = list(query_string))
    }
    
    query_id <- existing_query$query_id[1]
    
    # Link query to user
    dbExecute(pool, "
      INSERT INTO fact_user_queries_tracked (login_id, query_id, date_valid_from)
      VALUES ($1, $2, $3)
      ON CONFLICT (login_id, query_id, date_valid_from) DO NOTHING",
              params = list(login_id, query_id, Sys.Date()))
    
    # Link query to all of the user's active brands
    user_brands <- get_user_brands(login_id)
    
    for (i in seq_len(nrow(user_brands))) {
      bid <- user_brands$brand_id[i]
      dbExecute(pool, "
        INSERT INTO dim_brand_query (brand_id, query_id, date_added)
        VALUES ($1, $2, $3)
        ON CONFLICT (brand_id, query_id) DO NOTHING",
                params = list(bid, query_id, Sys.Date()))
    }
    
    return(list(
      query_id = query_id,
      needs_scoring = TRUE
    ))
  }, error = function(e) {
    warning(paste("Error in add_query_for_user:", e$message))
    return(NULL)
  })
}

#' Remove a tracked query for a user (soft delete)
remove_query_for_user <- function(login_id, query_id) {
  tryCatch({
    dbExecute(pool, "
      UPDATE fact_user_queries_tracked
      SET date_valid_to = CURRENT_DATE
      WHERE login_id = $1 
        AND query_id = $2
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
              params = list(login_id, query_id))
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error removing query:", e$message))
    return(FALSE)
  })
}

#' When a new competitor brand is added, link all user's existing queries to that brand
link_existing_queries_to_brand <- function(login_id, brand_id) {
  tryCatch({
    user_queries <- get_user_tracked_queries(login_id)
    
    for (i in seq_len(nrow(user_queries))) {
      qid <- user_queries$query_id[i]
      dbExecute(pool, "
        INSERT INTO dim_brand_query (brand_id, query_id, date_added)
        VALUES ($1, $2, $3)
        ON CONFLICT (brand_id, query_id) DO NOTHING",
                params = list(brand_id, qid, Sys.Date()))
    }
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error linking queries to brand:", e$message))
    return(FALSE)
  })
}

#' Check if query scores exist for all user's brands
query_has_scores_for_brands <- function(query_id, brand_ids) {
  if (length(brand_ids) == 0) return(TRUE)
  
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  query <- sprintf("
    SELECT COUNT(DISTINCT brand_id) as cnt
    FROM fact_query_history
    WHERE query_id = $1 AND brand_id IN (%s)
  ", placeholders)
  
  result <- dbGetQuery(pool, query, params = as.list(c(query_id, brand_ids)))
  return(result$cnt >= length(brand_ids))
}

