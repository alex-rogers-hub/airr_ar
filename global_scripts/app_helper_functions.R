# Helper function to fetch brand dashboard data
# CHANGED: added login_id parameter; all queries filter by it
get_brand_metrics <- function(brand_id, login_id) {
  queryDC <- "SELECT brand_id, brand_name
              FROM dim_brand
              WHERE brand_id = $1"
  
  queryA <- "SELECT * FROM fact_presence_history
             WHERE brand_id = $1 AND login_id = $2"
  
  queryB <- "SELECT * FROM fact_perception_history
             WHERE brand_id = $1 AND login_id = $2"
  
  queryC <- "SELECT * FROM fact_prestige_history
             WHERE brand_id = $1 AND login_id = $2"
  
  queryD <- "SELECT * FROM fact_persistence_history
             WHERE brand_id = $1 AND login_id = $2"
  
  queryE <- "SELECT * FROM fact_airr_history
             WHERE brand_id = $1 AND login_id = $2"
  
  brand_metrics <- list(
    brand      = dbGetQuery(pool, queryDC, params = list(brand_id)),
    presence   = dbGetQuery(pool, queryA,  params = list(brand_id, login_id)),
    perception = dbGetQuery(pool, queryB,  params = list(brand_id, login_id)),
    prestige   = dbGetQuery(pool, queryC,  params = list(brand_id, login_id)),
    persistence = dbGetQuery(pool, queryD, params = list(brand_id, login_id)),
    airr       = dbGetQuery(pool, queryE,  params = list(brand_id, login_id))
  )
  return(brand_metrics)
}

# Function to get latest score with specific column name
get_latest_score <- function(df, score_col) {
  if (nrow(df) == 0) return(0)
  latest_idx <- which.max(df$date)
  return(as.numeric(df[[score_col]][latest_idx]))
}

# Update addPrompt to use new con-explicit signature
addPrompt <- function(prompt_string, brand_id, login_id) {
  bg_con <- dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME"),
    host     = Sys.getenv("DB_HOST"),
    port     = Sys.getenv("DB_PORT"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  on.exit(dbDisconnect(bg_con), add = TRUE)
  
  tryCatch({
    existing_q <- dbGetQuery(pool,
                             "SELECT query_id FROM dim_query WHERE query_string = $1",
                             params = list(prompt_string))
    
    if (nrow(existing_q) == 0) {
      existing_q <- dbGetQuery(pool,
                               "INSERT INTO dim_query (query_string) VALUES ($1) RETURNING query_id",
                               params = list(prompt_string))
    }
    query_id <- existing_q$query_id[1]
    
    dbExecute(pool, "
      INSERT INTO dim_brand_query (brand_id, query_id, date_added)
      VALUES ($1, $2, $3)
      ON CONFLICT (brand_id, query_id) DO UPDATE SET date_added = EXCLUDED.date_added",
              params = list(brand_id, query_id, Sys.Date()))
    
    create_prompt_airr(bg_con, brand_id, prompt_string, query_id, login_id)
    
    return(query_id)
    
  }, error = function(e) {
    warning(paste("Error in addPrompt:", e$message))
    return(NULL)
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
  con_tx <- NULL
  
  tryCatch({
    cat("Step 1: Getting brands\n")
    con_tx <- poolCheckout(pool)
    dbBegin(con_tx)
    
    all_brands <- dbGetQuery(
      pool,
      "SELECT DISTINCT brand_id FROM dim_brand"
    )$brand_id
    cat("Found", length(all_brands), "brands\n")
    
    cat("Step 2: Starting transaction\n")
    
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
    dbCommit(con_tx)
    poolReturn(con_tx)
    con_tx <- NULL  # mark as returned
    
  }, error = function(e) {
    tryCatch({
      if (!is.null(con_tx)) {
        dbRollback(con_tx)
        poolReturn(con_tx)
      }
    }, error = function(e2) invisible(NULL))
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

# CHANGED: accepts and forwards industry
add_brand_for_user <- function(login_id, brand_name, 
                               main_brand = FALSE,
                               industry = NULL) {
  result <- add_brand_for_user_pending(login_id, brand_name, main_brand, industry)
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
    SELECT fus.*, ds.subscription_name, 
           ds.num_competitors_included,
           ds.num_prompts_included,
           ds.num_personas_included
    FROM fact_user_sub_level fus
    JOIN dim_subscription ds 
      ON fus.subscription_level_id = ds.subscription_level_id
    WHERE fus.login_id = $1
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    ORDER BY ds.num_competitors_included DESC
    LIMIT 1
  "
  result <- dbGetQuery(pool, query, params = list(login_id))
  
  if (nrow(result) == 0) {
    return(list(
      subscription_name        = "Free",
      num_competitors_included = 0,
      num_prompts_included     = 0,
      num_personas_included    = 0,
      extra_competitors_added  = 0,
      extra_prompts_added      = 0,
      max_brands               = 1
    ))
  }
  
  list(
    subscription_name        = result$subscription_name,
    num_competitors_included = result$num_competitors_included,
    num_prompts_included     = result$num_prompts_included,
    num_personas_included    = result$num_personas_included %||% 0,
    extra_competitors_added  = result$extra_competitors_added %||% 0,
    extra_prompts_added      = result$extra_prompts_added %||% 0,
    max_brands               = result$num_competitors_included + 
      (result$extra_competitors_added %||% 0) + 1
  )
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
# CHANGED: accepts industry parameter and stores it in fact_user_brands_tracked
add_brand_for_user_pending <- function(login_id, brand_name, 
                                       main_brand = FALSE,
                                       industry = NULL) {
  tryCatch({
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
    
    # Store the user's industry for this brand in the tracking table
    dbExecute(pool, "
      INSERT INTO fact_user_brands_tracked
        (login_id, brand_id, main_brand_flag, date_valid_from, industry)
      VALUES ($1, $2, $3, $4, $5)
      ON CONFLICT (login_id, brand_id, date_valid_from) DO UPDATE
        SET industry = EXCLUDED.industry",
              params = list(login_id, brand_id, main_brand, Sys.Date(),
                            if (is.null(industry) || !nzchar(industry)) NA else industry))
    
    has_scores <- brand_has_scores_for_user(brand_id, login_id)
    
    return(list(
      brand_id     = brand_id,
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

# NEW: checks scores for a specific (brand_id, login_id) pair
brand_has_scores_for_user <- function(brand_id, login_id) {
  result <- dbGetQuery(pool, "
    SELECT COUNT(*) as cnt
    FROM fact_airr_history
    WHERE brand_id = $1 AND login_id = $2",
                       params = list(brand_id, login_id))
  return(result$cnt > 0)
}

#' Get user's competitor brands with score status
# CHANGED: has_scores now checks (brand_id, login_id) pair
get_user_competitor_brands_with_status <- function(login_id) {
  query <- "
    SELECT b.brand_id, b.brand_name, ubt.date_valid_from, ubt.industry,
      CASE WHEN EXISTS (
        SELECT 1 FROM fact_airr_history fa
        WHERE fa.brand_id = b.brand_id
          AND fa.login_id = ubt.login_id     -- per-user check
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
    SELECT fus.*, ds.subscription_name,
           ds.num_competitors_included,
           ds.num_prompts_included,
           ds.num_personas_included
    FROM fact_user_sub_level fus
    JOIN dim_subscription ds
      ON fus.subscription_level_id = ds.subscription_level_id
    WHERE fus.login_id = $1
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    ORDER BY ds.num_competitors_included DESC
    LIMIT 1
  "
  result <- dbGetQuery(pool, query, params = list(login_id))
  
  if (nrow(result) == 0) {
    return(list(
      subscription_name        = "Free",
      num_competitors_included = 0,
      num_prompts_included     = 0,
      num_personas_included    = 0,
      extra_competitors_added  = 0,
      extra_prompts_added      = 0,
      extra_personas_added     = 0,
      max_brands               = 1,
      max_prompts              = 0
    ))
  }
  
  list(
    subscription_name        = result$subscription_name,
    num_competitors_included = result$num_competitors_included,
    num_prompts_included     = result$num_prompts_included,
    num_personas_included    = result$num_personas_included %||% 0,
    extra_competitors_added  = result$extra_competitors_added %||% 0,
    extra_prompts_added      = result$extra_prompts_added    %||% 0,
    extra_personas_added     = result$extra_personas_added   %||% 0,
    max_brands               = result$num_competitors_included +
      (result$extra_competitors_added %||% 0) + 1,
    max_prompts              = result$num_prompts_included +
      (result$extra_prompts_added %||% 0)
  )
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

#' Build a reach context string for use in prompts
#' @param reach Character: "global", "national", "regional", or "near_me"
#' @param country Character or NULL/NA
#' @param region Character or NULL/NA
#' @param postcode Character or NULL/NA
#' @return Character string (empty for global)
build_reach_context <- function(reach, country = NULL, region = NULL, postcode = NULL) {
  if (is.null(reach) || is.na(reach)) return("")
  
  # Clean up NAs
  if (!is.null(country)  && is.na(country))  country  <- NULL
  if (!is.null(region)   && is.na(region))   region   <- NULL
  if (!is.null(postcode) && is.na(postcode)) postcode <- NULL
  
  switch(reach,
         "global"   = "",
         "national" = if (!is.null(country) && nzchar(country)) country else "",
         "regional" = if (!is.null(region)  && nzchar(region))  region  else "",
         "near_me"  = {
           parts <- c()
           if (!is.null(postcode) && nzchar(postcode)) parts <- c(parts, postcode)
           if (!is.null(country)  && nzchar(country))  parts <- c(parts, country)
           paste(parts, collapse = ", ")
         },
         ""
  )
}

#' Build a "near me" suffix for prompt queries
#' For near_me brands, replaces generic prompts with location-specific ones
#' @param reach Character
#' @param country Character or NULL/NA
#' @param postcode Character or NULL/NA
#' @return Character string like "near 10001, United States" or ""
build_near_me_suffix <- function(reach, country = NULL, postcode = NULL) {
  if (is.null(reach) || is.na(reach) || reach != "near_me") return("")
  
  if (!is.null(country)  && is.na(country))  country  <- NULL
  if (!is.null(postcode) && is.na(postcode)) postcode <- NULL
  
  parts <- c()
  if (!is.null(postcode) && nzchar(postcode)) parts <- c(parts, postcode)
  if (!is.null(country)  && nzchar(country))  parts <- c(parts, country)
  
  if (length(parts) > 0) {
    return(paste0(" near ", paste(parts, collapse = ", ")))
  }
  return("")
}

#' Deduplicate fact_user_brands_tracked
#' Keeps the oldest date_valid_from per login_id + brand_id combination
dedupe_user_brands_tracked <- function(login_id) {
  tryCatch({
    dbExecute(pool, "
      DELETE FROM fact_user_brands_tracked a
      USING fact_user_brands_tracked b
      WHERE a.login_id = b.login_id
        AND a.brand_id = b.brand_id
        AND a.date_valid_from > b.date_valid_from
        AND a.login_id = $1;
    ", params = list(login_id))
    
    message(sprintf("✓ Deduped fact_user_brands_tracked for login_id %d", login_id))
    return(TRUE)
  }, error = function(e) {
    warning(paste("Error deduping user brands:", e$message))
    return(FALSE)
  })
}

#' Sanitise text from LLM responses
#' Converts to UTF-8, replaces problematic Unicode chars, removes non-printable chars
#' @param text Character vector
#' @return Cleaned character vector
sanitise_text <- function(text) {
  if (is.null(text)) return(text)
  
  text <- as.character(text)
  text <- enc2utf8(text)
  
  replacements <- c(
    "\u00e4" = "a",  "\u00f6" = "o",  "\u00fc" = "u",
    "\u00c4" = "A",  "\u00d6" = "O",  "\u00dc" = "U",
    "\u00df" = "ss", "\u00e9" = "e",  "\u00e8" = "e",
    "\u00ea" = "e",  "\u00eb" = "e",  "\u00c9" = "E",
    "\u00c8" = "E",  "\u00ca" = "E",  "\u00cb" = "E",
    "\u00e0" = "a",  "\u00e1" = "a",  "\u00e2" = "a",
    "\u00e3" = "a",  "\u00c0" = "A",  "\u00c1" = "A",
    "\u00c2" = "A",  "\u00c3" = "A",  "\u00ec" = "i",
    "\u00ed" = "i",  "\u00ee" = "i",  "\u00ef" = "i",
    "\u00cc" = "I",  "\u00cd" = "I",  "\u00ce" = "I",
    "\u00cf" = "I",  "\u00f2" = "o",  "\u00f3" = "o",
    "\u00f4" = "o",  "\u00f5" = "o",  "\u00d2" = "O",
    "\u00d3" = "O",  "\u00d4" = "O",  "\u00d5" = "O",
    "\u00f9" = "u",  "\u00fa" = "u",  "\u00fb" = "u",
    "\u00d9" = "U",  "\u00da" = "U",  "\u00db" = "U",
    "\u00f1" = "n",  "\u00d1" = "N",  "\u00e7" = "c",
    "\u00c7" = "C",  "\u00fd" = "y",  "\u00ff" = "y",
    "\u0160" = "S",  "\u0161" = "s",  "\u017d" = "Z",
    "\u017e" = "z",  "\u010c" = "C",  "\u010d" = "c",
    "\u0159" = "r",  "\u0158" = "R",  "\u0142" = "l",
    "\u0141" = "L",  "\u00f8" = "o",  "\u00d8" = "O",
    "\u00e5" = "a",  "\u00c5" = "A",  "\u00e6" = "ae",
    "\u00c6" = "AE", "\u2019" = "'",  "\u2018" = "'",
    "\u201c" = '"',  "\u201d" = '"',  "\u2013" = "-",
    "\u2014" = "-",  "\u2026" = "...","\u00a0" = " ",
    "\u200b" = "",   "\u200c" = "",   "\u200d" = "",
    "\ufeff" = ""
  )
  
  for (from in names(replacements)) {
    text <- gsub(from, replacements[[from]], text, fixed = TRUE)
  }
  
  # Remove any remaining non-ASCII
  text <- gsub("[^\x20-\x7E\n\r\t]", "", text)
  
  # Clean up multiple spaces
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  return(text)
}

# ============================================
# Subscription helpers
# ============================================

#' Upsert an Enterprise subscription for a user
#' Safe to call on every login — only inserts if none exists
ensure_enterprise_subscription <- function(login_id) {
  tryCatch({
    
    # Get Enterprise subscription_level_id
    ent <- dbGetQuery(pool,
                      "SELECT subscription_level_id 
       FROM dim_subscription 
       WHERE subscription_name = 'Enterprise'
       LIMIT 1")
    
    if (nrow(ent) == 0) {
      warning("Enterprise subscription tier not found in dim_subscription")
      return(invisible(FALSE))
    }
    
    ent_id <- ent$subscription_level_id[1]
    
    # Close off any existing active subscriptions that aren't Enterprise
    dbExecute(pool,
              "UPDATE fact_user_sub_level
       SET date_valid_to = CURRENT_DATE - 1
       WHERE login_id = $1
         AND subscription_level_id != $2
         AND date_valid_to >= CURRENT_DATE",
              params = list(login_id, ent_id))
    
    # Insert Enterprise if not already active
    dbExecute(pool,
              "INSERT INTO fact_user_sub_level
         (login_id, subscription_level_id, date_valid_from, date_valid_to)
       VALUES ($1, $2, CURRENT_DATE, '2099-12-31')
       ON CONFLICT DO NOTHING",
              params = list(login_id, ent_id))
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    warning(paste("ensure_enterprise_subscription failed:", e$message))
    return(invisible(FALSE))
  })
}

ensure_free_subscription <- function(login_id) {
  tryCatch({
    
    free_id <- dbGetQuery(pool,
                          "SELECT subscription_level_id FROM dim_subscription
       WHERE subscription_name = 'Free' LIMIT 1")$subscription_level_id[1]
    
    # Only insert if no active subscription exists at all
    existing <- dbGetQuery(pool,
                           "SELECT COUNT(*) as cnt FROM fact_user_sub_level
       WHERE login_id = $1
         AND date_valid_from <= CURRENT_DATE
         AND date_valid_to >= CURRENT_DATE",
                           params = list(login_id))$cnt
    
    if (existing == 0) {
      dbExecute(pool,
                "INSERT INTO fact_user_sub_level
           (login_id, subscription_level_id, date_valid_from, date_valid_to)
         VALUES ($1, $2, CURRENT_DATE, '2099-12-31')
         ON CONFLICT DO NOTHING",
                params = list(login_id, free_id))
    }
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    warning(paste("ensure_free_subscription failed:", e$message))
    return(invisible(FALSE))
  })
}

estimate_setup_time <- function(n_competitors, n_prompts, n_personas) {
  
  n_brands <- n_competitors + 1  # +1 for main brand
  
  # Brand scoring (sequential, ~90s each)
  brand_time <- n_brands * 90
  
  # Prompt scoring (~15s per brand per prompt, but batched per prompt)
  prompt_time <- n_prompts * n_brands * 15
  
  # Persona scoring (~30s per brand per persona for brand-level)
  persona_brand_time <- n_personas * n_brands * 30
  
  # Persona prompt scoring (~15s per brand per prompt per persona)
  persona_prompt_time <- n_personas * n_prompts * n_brands * 15
  
  total_seconds <- brand_time + prompt_time + persona_brand_time + persona_prompt_time
  
  # Format nicely
  if (total_seconds < 120) {
    time_str <- paste0("about ", round(total_seconds / 60), " minute")
  } else if (total_seconds < 3600) {
    mins <- round(total_seconds / 60)
    time_str <- paste0("about ", mins, " minutes")
  } else {
    hours <- floor(total_seconds / 3600)
    mins  <- round((total_seconds %% 3600) / 60)
    time_str <- paste0("about ", hours, "h ", mins, "m")
  }
  
  list(
    total_seconds      = total_seconds,
    time_str           = time_str,
    brand_seconds      = brand_time,
    prompt_seconds     = prompt_time,
    persona_seconds    = persona_brand_time + persona_prompt_time
  )
}

# ============================================
# Timing notice helpers — for post-onboarding additions
# ============================================

#' Estimate time to score a single new competitor brand
#' (brand scoring + all existing prompts + all existing personas)
estimate_competitor_add_time <- function(login_id) {
  n_prompts  <- get_user_query_count(login_id)
  
  # Count active personas
  n_personas <- tryCatch({
    dbGetQuery(pool, "
      SELECT COUNT(*) as cnt
      FROM fact_user_profiles_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
    ", params = list(login_id))$cnt
  }, error = function(e) 0)
  
  # 1 new brand
  brand_time         <- 90
  # Each prompt needs scoring for the new brand
  prompt_time        <- n_prompts * 15
  # Each persona needs brand-level + prompt-level scoring for new brand
  persona_brand_time <- n_personas * 30
  persona_prompt_time <- n_personas * n_prompts * 15
  
  total_seconds <- brand_time + prompt_time + persona_brand_time + persona_prompt_time
  
  list(
    total_seconds = total_seconds,
    time_str      = format_seconds_to_str(total_seconds),
    breakdown     = list(
      brand   = brand_time,
      prompts = prompt_time,
      persona = persona_brand_time + persona_prompt_time
    )
  )
}

#' Estimate time to score a single new prompt
#' (all existing brands + all existing personas × all existing brands)
estimate_prompt_add_time <- function(login_id) {
  n_brands <- tryCatch({
    dbGetQuery(pool, "
      SELECT COUNT(*) as cnt
      FROM fact_user_brands_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
    ", params = list(login_id))$cnt
  }, error = function(e) 1)
  
  n_personas <- tryCatch({
    dbGetQuery(pool, "
      SELECT COUNT(*) as cnt
      FROM fact_user_profiles_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
    ", params = list(login_id))$cnt
  }, error = function(e) 0)
  
  # 1 new prompt × all brands
  prompt_time         <- n_brands * 15
  # Each persona needs this prompt scored for each brand
  persona_prompt_time <- n_personas * n_brands * 15
  
  total_seconds <- prompt_time + persona_prompt_time
  
  list(
    total_seconds = total_seconds,
    time_str      = format_seconds_to_str(total_seconds),
    breakdown     = list(
      brands  = prompt_time,
      persona = persona_prompt_time
    )
  )
}

#' Estimate time to score a single new persona
#' (all existing brands + all existing prompts × all existing brands)
estimate_persona_add_time <- function(login_id) {
  n_brands <- tryCatch({
    dbGetQuery(pool, "
      SELECT COUNT(*) as cnt
      FROM fact_user_brands_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
    ", params = list(login_id))$cnt
  }, error = function(e) 1)
  
  n_prompts <- get_user_query_count(login_id)
  
  # Brand-level scoring for each brand
  persona_brand_time  <- n_brands * 30
  # Prompt-level scoring for each brand × each prompt
  persona_prompt_time <- n_brands * n_prompts * 15
  
  total_seconds <- persona_brand_time + persona_prompt_time
  
  list(
    total_seconds = total_seconds,
    time_str      = format_seconds_to_str(total_seconds),
    breakdown     = list(
      brands  = persona_brand_time,
      prompts = persona_prompt_time
    )
  )
}

#' Format seconds into a human-readable string
format_seconds_to_str <- function(total_seconds) {
  if (total_seconds < 60) {
    "under a minute"
  } else if (total_seconds < 120) {
    "about 1 minute"
  } else if (total_seconds < 3600) {
    paste0("about ", round(total_seconds / 60), " minutes")
  } else {
    hours <- floor(total_seconds / 3600)
    mins  <- round((total_seconds %% 3600) / 60)
    if (mins == 0) {
      paste0("about ", hours, "h")
    } else {
      paste0("about ", hours, "h ", mins, "m")
    }
  }
}

#' Render a timing notice div
#' @param time_str  Character — formatted time string from estimate_*_add_time()
#' @param breakdown Named list with per-step seconds (optional, NULL to hide)
#' @param item_label Character — "competitor", "prompt", or "persona"
render_timing_notice <- function(time_str, breakdown = NULL, item_label = "item") {
  
  detail_rows <- if (!is.null(breakdown)) {
    tagList(
      lapply(names(breakdown), function(key) {
        secs <- breakdown[[key]]
        if (secs == 0) return(NULL)
        div(
          style = "display: flex; justify-content: space-between;
                   font-size: 11px; color: #718096; margin-top: 3px;",
          tags$span(paste0("Scoring across ", key)),
          tags$span(paste0("~", round(secs / 60), " min"))
        )
      })
    )
  } else NULL
  
  div(
    style = "background: rgba(102,126,234,0.06);
             border: 1px solid rgba(102,126,234,0.2);
             border-radius: 8px; padding: 10px 14px; margin-top: 10px;",
    div(
      style = "display: flex; align-items: center; gap: 7px;",
      icon("clock", style = "color: #667eea; font-size: 13px; flex-shrink: 0;"),
      tags$span(
        style = "font-size: 13px; font-weight: 600; color: #2d3748;",
        paste0("Scores will be ready in ", time_str)
      )
    ),
    if (!is.null(detail_rows)) {
      div(style = "margin-top: 6px;", detail_rows)
    },
    div(
      style = "margin-top: 7px; font-size: 11px; color: #a0aec0;",
      icon("info-circle", style = "margin-right: 3px;"),
      paste0(
        "Your new ", item_label, " will be scored in the background. ",
        "Results appear automatically — no need to stay on this page."
      )
    )
  )
}

#' Update industry for a user-brand pairing
update_user_brand_industry <- function(login_id, industry) {
  tryCatch({
    
    # Update all active user-brand rows for this user
    dbExecute(pool, "
      UPDATE fact_user_brands_tracked
      SET industry = $1
      WHERE login_id = $2
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
              params = list(industry, login_id))
    
    # Update dim_brand for all brands this user tracks
    brand_ids <- dbGetQuery(pool, "
      SELECT DISTINCT brand_id
      FROM fact_user_brands_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
                            params = list(login_id))$brand_id
    
    if (length(brand_ids) > 0) {
      placeholders <- paste0("$", seq_along(brand_ids) + 1, collapse = ", ")
      dbExecute(pool,
                sprintf("UPDATE dim_brand SET industry = $1 WHERE brand_id IN (%s)",
                        placeholders),
                params = as.list(c(industry, brand_ids)))
    }
    
    return(list(success = TRUE, brand_ids = brand_ids))
    
  }, error = function(e) {
    warning(paste("Error updating industry:", e$message))
    return(list(success = FALSE, brand_ids = c()))
  })
}

#' Estimate time to rescore all brands for a user
#' (same as full setup minus onboarding overhead)
estimate_rescore_time <- function(login_id) {
  
  n_brands <- tryCatch({
    dbGetQuery(pool, "
      SELECT COUNT(*) as cnt
      FROM fact_user_brands_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
               params = list(login_id))$cnt
  }, error = function(e) 1)
  
  n_prompts <- get_user_query_count(login_id)
  
  n_personas <- tryCatch({
    dbGetQuery(pool, "
      SELECT COUNT(*) as cnt
      FROM fact_user_profiles_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
               params = list(login_id))$cnt
  }, error = function(e) 0)
  
  brand_time          <- n_brands * 90
  prompt_time         <- n_prompts * n_brands * 15
  persona_brand_time  <- n_personas * n_brands * 30
  persona_prompt_time <- n_personas * n_prompts * n_brands * 15
  
  total_seconds <- brand_time + prompt_time + persona_brand_time + persona_prompt_time
  
  list(
    total_seconds = total_seconds,
    time_str      = format_seconds_to_str(total_seconds),
    n_brands      = n_brands,
    n_prompts     = n_prompts,
    n_personas    = n_personas,
    breakdown     = list(
      brands  = brand_time,
      prompts = prompt_time,
      persona = persona_brand_time + persona_prompt_time
    )
  )
}

#' Update brand reach for all of a user's brands
update_user_brand_reach <- function(login_id, brand_reach,
                                    reach_country  = NULL,
                                    reach_region   = NULL,
                                    reach_postcode = NULL) {
  tryCatch({
    
    # Clean up empty strings to NULL
    if (!is.null(reach_country)  && !nzchar(reach_country))  reach_country  <- NULL
    if (!is.null(reach_region)   && !nzchar(reach_region))   reach_region   <- NULL
    if (!is.null(reach_postcode) && !nzchar(reach_postcode)) reach_postcode <- NULL
    
    # Get all brand IDs this user tracks
    brand_ids <- dbGetQuery(pool, "
      SELECT DISTINCT brand_id
      FROM fact_user_brands_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
                            params = list(login_id))$brand_id
    
    if (length(brand_ids) == 0) {
      return(list(success = TRUE, brand_ids = c()))
    }
    
    # Update dim_brand for all brands this user tracks
    # (reach lives on dim_brand, not fact_user_brands_tracked)
    placeholders <- paste0("$", seq_along(brand_ids) + 4, collapse = ", ")
    dbExecute(pool,
              sprintf("
        UPDATE dim_brand
        SET brand_reach    = $1,
            reach_country  = $2,
            reach_region   = $3,
            reach_postcode = $4
        WHERE brand_id IN (%s)", placeholders),
              params = as.list(c(
                brand_reach,
                reach_country  %||% NA,
                reach_region   %||% NA,
                reach_postcode %||% NA,
                brand_ids
              )))
    
    return(list(success = TRUE, brand_ids = brand_ids))
    
  }, error = function(e) {
    warning(paste("Error updating reach:", e$message))
    return(list(success = FALSE, brand_ids = c()))
  })
}

#' Format reach for display
#' e.g. "National — United Kingdom" or "Near Me — SW1A 1AA, UK"
format_reach_display <- function(brand_reach, reach_country = NULL,
                                 reach_region = NULL, reach_postcode = NULL) {
  if (is.null(brand_reach) || is.na(brand_reach) || brand_reach == "global") {
    return("Global")
  }
  
  label <- switch(brand_reach,
                  "national" = "National",
                  "regional" = "Regional",
                  "near_me"  = "Near Me",
                  "Global"
  )
  
  detail <- switch(brand_reach,
                   "national" = if (!is.null(reach_country) && !is.na(reach_country) && 
                                    nzchar(reach_country)) reach_country else NULL,
                   "regional" = if (!is.null(reach_region)  && !is.na(reach_region)  && 
                                    nzchar(reach_region))  reach_region  else NULL,
                   "near_me"  = {
                     parts <- c()
                     if (!is.null(reach_postcode) && !is.na(reach_postcode) && nzchar(reach_postcode))
                       parts <- c(parts, reach_postcode)
                     if (!is.null(reach_country)  && !is.na(reach_country)  && nzchar(reach_country))
                       parts <- c(parts, reach_country)
                     if (length(parts) > 0) paste(parts, collapse = ", ") else NULL
                   },
                   NULL
  )
  
  if (!is.null(detail)) paste0(label, " \u2014 ", detail) else label
}

# ============================================
# API Key helpers
# ============================================

#' Generate a new API key for a user
generate_api_key <- function(login_id, key_name = "Default") {
  
  # Check enterprise subscription
  # API access available on Pro and Enterprise
  if (!sub$subscription_name %in% c("Pro", "Enterprise")) {
    return(list(success = FALSE,
                message = "API access requires a Pro or Enterprise plan."))
  }
  
  # Limit to 5 active keys per user
  existing <- dbGetQuery(pool, "
    SELECT COUNT(*) as cnt FROM dim_api_keys
    WHERE login_id = $1 AND is_active = TRUE",
                         params = list(login_id))
  
  if (existing$cnt >= 5) {
    return(list(success = FALSE, 
                message = "Maximum of 5 active API keys reached. Revoke one first."))
  }
  
  # Generate a secure key: "airr_" + 48 random hex chars
  raw     <- paste0(sample(c(letters, LETTERS, 0:9), 48, replace = TRUE), 
                    collapse = "")
  api_key <- paste0("airr_", raw)
  
  tryCatch({
    dbExecute(pool, "
      INSERT INTO dim_api_keys (login_id, api_key, key_name, date_created)
      VALUES ($1, $2, $3, CURRENT_DATE)",
              params = list(login_id, api_key, key_name))
    
    return(list(success = TRUE, api_key = api_key))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Revoke an API key
revoke_api_key <- function(login_id, api_key_id) {
  tryCatch({
    rows <- dbExecute(pool, "
      UPDATE dim_api_keys
      SET is_active = FALSE, date_revoked = NOW()
      WHERE api_key_id = $1 AND login_id = $2 AND is_active = TRUE",
                      params = list(api_key_id, login_id))
    
    return(rows > 0)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Get all active API keys for a user (never returns the key itself after creation)
get_user_api_keys <- function(login_id) {
  dbGetQuery(pool, "
    SELECT api_key_id, key_name, date_created, date_last_used,
           LEFT(api_key, 12) || '...' as key_preview
    FROM dim_api_keys
    WHERE login_id = $1 AND is_active = TRUE
    ORDER BY date_created DESC",
             params = list(login_id))
}

#' Validate an API key and return login_id + check enterprise
#' Used by the Plumber API
validate_api_key <- function(api_key, con) {
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
    WHERE k.api_key = $1
      AND k.is_active = TRUE",
                       params = list(api_key))
  
  if (nrow(result) == 0) return(NULL)
  if (result$subscription_name[1] != "Enterprise") return(NULL)
  
  # Update last used timestamp
  dbExecute(con, "
    UPDATE dim_api_keys SET date_last_used = NOW()
    WHERE api_key_id = $1",
            params = list(result$api_key_id[1]))
  
  return(result)
}

is_demo_session <- function(rv) {
  isTRUE(rv$is_demo)
}

# ============================================
# Password reset helpers
# ============================================

#' Validate password strength
#' Returns NULL if ok, or an error message string
validate_password_strength <- function(password) {
  if (nchar(password) < 8) {
    return("Password must be at least 8 characters.")
  }
  if (!grepl("[A-Z]", password)) {
    return("Password must contain at least one capital letter.")
  }
  if (!grepl("[0-9]", password)) {
    return("Password must contain at least one number.")
  }
  if (!grepl("[^A-Za-z0-9]", password)) {
    return("Password must contain at least one special character (e.g. !@#$).")
  }
  return(NULL)
}

#' Generate a password reset token and store it
generate_reset_token <- function(email) {
  
  user <- dbGetQuery(pool,
                     "SELECT login_id FROM dim_user WHERE email = $1",
                     params = list(tolower(trimws(email))))
  
  # Email not found — return structured response not NULL
  if (nrow(user) == 0) {
    return(list(success = FALSE, token = NULL))
  }
  
  login_id <- user$login_id[1]
  token    <- paste0(sample(c(letters, LETTERS, 0:9), 48, replace = TRUE),
                     collapse = "")
  expires  <- Sys.time() + 3600
  
  # Delete any existing tokens for this user
  dbExecute(pool,
            "DELETE FROM dim_password_reset_tokens WHERE login_id = $1",
            params = list(login_id))
  
  dbExecute(pool,
            "INSERT INTO dim_password_reset_tokens
       (login_id, token, expires_at, used)
     VALUES ($1, $2, $3, FALSE)",
            params = list(login_id, token, expires))
  
  return(list(success = TRUE, token = token, login_id = login_id))
}

#' Validate a reset token
validate_reset_token <- function(token) {
  result <- dbGetQuery(pool, "
    SELECT t.token_id, t.login_id, u.email
    FROM dim_password_reset_tokens t
    JOIN dim_user u ON u.login_id = t.login_id
    WHERE t.token = $1
      AND t.used = FALSE
      AND t.expires_at > NOW()",
                       params = list(token))
  
  if (nrow(result) == 0) return(NULL)
  result
}

#' Apply a password reset
apply_password_reset <- function(token, new_password) {
  
  token_row <- validate_reset_token(token)
  if (is.null(token_row)) {
    return(list(success = FALSE,
                message = "Reset link is invalid or has expired."))
  }
  
  # Update password
  dbExecute(pool, "
    UPDATE dim_user SET password_hash = $1 WHERE login_id = $2",
            params = list(hash_password(new_password), token_row$login_id[1]))
  
  # Mark token used
  dbExecute(pool, "
    UPDATE dim_password_reset_tokens SET used = TRUE
    WHERE token_id = $1",
            params = list(token_row$token_id[1]))
  
  return(list(success = TRUE, email = token_row$email[1]))
}

#' Send password reset email
#' Uses basic SMTP — configure env vars for your email provider
send_reset_email <- function(to_email, reset_token, app_url) {
  
  reset_url <- paste0(app_url, "?reset_token=", reset_token)
  
  subject <- "Reset your AiRR password"
  
  body <- paste0(
    "Hi,\n\n",
    "You requested a password reset for your AiRR account.\n\n",
    "Click the link below to reset your password. ",
    "This link expires in 1 hour.\n\n",
    reset_url, "\n\n",
    "If you didn't request this, you can safely ignore this email.\n\n",
    "The AiRR Team\n",
    "airrscore.com"
  )
  
  tryCatch({
    
    smtp_server   <- Sys.getenv("SMTP_HOST")
    smtp_port     <- as.integer(Sys.getenv("SMTP_PORT") %||% "587")
    smtp_user     <- Sys.getenv("SMTP_USER")
    smtp_password <- Sys.getenv("SMTP_PASSWORD")
    from_email    <- Sys.getenv("SMTP_FROM") %||% "noreply@airrscore.com"
    
    if (!nzchar(smtp_server)) {
      message("SMTP not configured — reset URL: ", reset_url)
      return(list(success = TRUE, url = reset_url))
    }
    
    # Use curl to send via SMTP
    system2(
      "curl",
      args = c(
        "--ssl-reqd",
        paste0("--url 'smtps://", smtp_server, ":", smtp_port, "'"),
        paste0("--user '", smtp_user, ":", smtp_password, "'"),
        paste0("--mail-from '", from_email, "'"),
        paste0("--mail-rcpt '", to_email, "'"),
        "--upload-file -"
      ),
      input = c(
        paste0("From: AiRR Password Reset <", smtp_user, ">"),
        paste0("To: ", to_email),
        paste0("Subject: ", subject),
        "",
        body
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    
    return(list(success = TRUE, url = reset_url))
    
  }, error = function(e) {
    message("Email send failed: ", e$message)
    message("Reset URL: ", reset_url)
    return(list(success = TRUE, url = reset_url))  # still succeed
  })
}

#' Reset a demo onboarding account back to vanilla state
#' Removes all brands, competitors, prompts, personas and marks
#' onboarding as incomplete so they go through setup again next login
reset_onboarding_account <- function(login_id) {
  tryCatch({
    
    message(sprintf("=== Resetting onboarding account: login_id %d ===", login_id))
    
    # 1. Hard-delete score history first (before removing brand tracking rows
    #    so the USING join in query_history still works)
    dbExecute(pool, "
      DELETE FROM fact_airr_history
      WHERE login_id = $1",
              params = list(login_id))
    
    dbExecute(pool, "
      DELETE FROM fact_presence_history
      WHERE login_id = $1",
              params = list(login_id))
    
    dbExecute(pool, "
      DELETE FROM fact_perception_history
      WHERE login_id = $1",
              params = list(login_id))
    
    dbExecute(pool, "
      DELETE FROM fact_prestige_history
      WHERE login_id = $1",
              params = list(login_id))
    
    dbExecute(pool, "
      DELETE FROM fact_persistence_history
      WHERE login_id = $1",
              params = list(login_id))
    
    # Query history — join to brands tracked to find this user's records
    dbExecute(pool, "
      DELETE FROM fact_query_history
      WHERE brand_id IN (
        SELECT brand_id FROM fact_user_brands_tracked
        WHERE login_id = $1
      )",
              params = list(login_id))
    
    message("  ✓ Score history cleared")
    
    # 2. Hard-delete brand tracking
    dbExecute(pool, "
      DELETE FROM fact_user_brands_tracked
      WHERE login_id = $1",
              params = list(login_id))
    message("  ✓ Brands cleared")
    
    # 3. Hard-delete prompt tracking
    dbExecute(pool, "
      DELETE FROM fact_user_queries_tracked
      WHERE login_id = $1",
              params = list(login_id))
    message("  ✓ Prompts cleared")
    
    # 4. Hard-delete personas
    dbExecute(pool, "
      DELETE FROM fact_user_profiles_tracked
      WHERE login_id = $1",
              params = list(login_id))
    message("  ✓ Personas cleared")
    
    # 5. Delete pending job queue entries
    dbExecute(pool, "
      DELETE FROM dim_job_queue
      WHERE login_id = $1
        AND status = 'pending'",
              params = list(login_id))
    message("  ✓ Pending jobs cleared")
    
    # 6. Mark onboarding as incomplete
    dbExecute(pool, "
      UPDATE dim_user
      SET onboarding_complete = FALSE
      WHERE login_id = $1",
              params = list(login_id))
    message("  ✓ Onboarding reset")
    
    message("=== Onboarding account reset complete ===")
    return(invisible(TRUE))
    
  }, error = function(e) {
    message(sprintf("✗ Reset failed for login_id %d: %s", login_id, e$message))
    return(invisible(FALSE))
  })
}

#' Get all aliases for a brand (returns character vector including brand name)
get_brand_search_names <- function(brand_id, brand_name, con = pool) {
  aliases <- tryCatch(
    dbGetQuery(con, "
      SELECT alias_name FROM dim_brand_aliases
      WHERE brand_id = $1
      ORDER BY alias_id",
               params = list(brand_id))$alias_name,
    error = function(e) character(0)
  )
  # Always include the main brand name first
  unique(c(brand_name, aliases))
}

#' Add an alias for a brand
add_brand_alias <- function(brand_id, alias_name) {
  tryCatch({
    dbExecute(pool, "
      INSERT INTO dim_brand_aliases (brand_id, alias_name)
      VALUES ($1, $2)
      ON CONFLICT (brand_id, alias_name) DO NOTHING",
              params = list(brand_id, trimws(alias_name)))
    return(TRUE)
  }, error = function(e) {
    warning("Error adding alias: ", e$message)
    return(FALSE)
  })
}

#' Remove an alias
remove_brand_alias <- function(alias_id) {
  tryCatch({
    dbExecute(pool,
              "DELETE FROM dim_brand_aliases WHERE alias_id = $1",
              params = list(alias_id))
    return(TRUE)
  }, error = function(e) {
    warning("Error removing alias: ", e$message)
    return(FALSE)
  })
}

#' Get aliases for a brand with IDs (for UI display)
get_brand_aliases <- function(brand_id) {
  tryCatch(
    dbGetQuery(pool, "
      SELECT alias_id, alias_name, date_added
      FROM dim_brand_aliases
      WHERE brand_id = $1
      ORDER BY alias_id",
               params = list(brand_id)),
    error = function(e) data.frame()
  )
}


#' Get all linked accounts for a primary user
get_linked_accounts <- function(login_id) {
  dbGetQuery(pool, "
    SELECT 
      u.login_id,
      u.email,
      u.onboarding_complete,
      COALESCE(u.account_name, b.brand_name, u.email) AS display_name,
      b.brand_name,
      ds.subscription_name
    FROM dim_user u
    LEFT JOIN fact_user_brands_tracked ubt
      ON ubt.login_id = u.login_id
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LEFT JOIN dim_brand b ON b.brand_id = ubt.brand_id
    LEFT JOIN fact_user_sub_level fus
      ON fus.login_id = u.login_id
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    LEFT JOIN dim_subscription ds
      ON ds.subscription_level_id = fus.subscription_level_id
    WHERE u.primary_login_id = $1
      AND u.is_linked_account = TRUE
    ORDER BY u.login_id",
             params = list(login_id))
}

#' Get the primary account for a linked user
get_primary_account <- function(login_id) {
  dbGetQuery(pool, "
    SELECT u2.login_id, u2.email, b.brand_name
    FROM dim_user u1
    JOIN dim_user u2 ON u2.login_id = u1.primary_login_id
    LEFT JOIN fact_user_brands_tracked ubt
      ON ubt.login_id = u2.login_id
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LEFT JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE u1.login_id = $1",
             params = list(login_id))
}


#' @param primary_email     Email of the main account to link to
#' @param link_number       Which link number (1, 2, 3...) — determines email prefix
#' @param password          Password for the new linked account
#' @param n_competitors     Competitor slots for this account
#' @param n_prompts         Prompt slots for this account
#' @param n_personas        Persona slots for this account
#' @param brand_name        Optional — pre-set the brand name
#'
create_linked_account <- function(primary_email,
                                  link_number   = 1,
                                  password      = NULL,
                                  n_competitors = 10,
                                  n_prompts     = 10,
                                  n_personas    = 3,
                                  account_name   = NULL, 
                                  brand_name    = NULL) {
  
  # ── Find primary account ─────────────────────────────────────────────
  primary <- dbGetQuery(pool,
                        "SELECT login_id, email FROM dim_user WHERE email = $1",
                        params = list(tolower(trimws(primary_email))))
  
  if (nrow(primary) == 0) {
    cat(sprintf("✗ Primary account not found: %s\n", primary_email))
    return(invisible(NULL))
  }
  
  primary_login_id <- primary$login_id[1]
  cat(sprintf("✓ Primary account found: %s (login_id: %d)\n",
              primary_email, primary_login_id))
  
  # ── Build linked email address ────────────────────────────────────────
  # Split primary email at @ and prepend link prefix
  # Clean the primary email before splitting
  primary_email_clean <- tolower(trimws(primary_email))
  email_parts  <- strsplit(primary_email, "@")[[1]]
  # Remove any whitespace from both parts before concatenating
  local_part  <- trimws(email_parts[1])
  domain_part <- trimws(email_parts[2])
  
  linked_email <- sprintf("link%d.%s@%s", link_number, local_part, domain_part)
  
  # Final safety clean — remove any remaining whitespace
  linked_email <- gsub("\\s+", "", linked_email)
  
  cat(sprintf("  Linked email: %s\n", linked_email))
  
  # Check it doesn't already exist
  existing <- dbGetQuery(pool,
                         "SELECT login_id FROM dim_user WHERE email = $1",
                         params = list(linked_email))
  
  if (nrow(existing) > 0) {
    cat(sprintf("! Linked account already exists (login_id: %d)\n",
                existing$login_id[1]))
    linked_login_id <- existing$login_id[1]
  } else {
    # ── Create the linked user account ─────────────────────────────────
    if (is.null(password)) {
      # Generate a random password if none supplied
      password <- paste0(
        sample(c(LETTERS, letters, 0:9, "!", "@", "#"), 12, replace = TRUE),
        collapse = "")
    }
    
    password_hash <- digest::digest(password, algo = "sha256")
    
    new_user <- dbGetQuery(pool,
                           "INSERT INTO dim_user
       (date_added, email, password_hash, onboarding_complete,
        is_linked_account, primary_login_id, account_name)
     VALUES (CURRENT_DATE, $1, $2, FALSE, TRUE, $3, $4)
     RETURNING login_id",
                           params = list(linked_email, password_hash, primary_login_id,
                                         account_name %||% linked_email))
    
    linked_login_id <- new_user$login_id[1]
    cat(sprintf("✓ Linked user created (login_id: %d)\n", linked_login_id))
  }
  
  # ── Determine subscription — Enterprise base + extra slots if needed ──
  ent_sub <- dbGetQuery(pool,
                        "SELECT subscription_level_id, num_competitors_included,
            num_prompts_included, num_personas_included
     FROM dim_subscription
     WHERE subscription_name = 'Enterprise' LIMIT 1")
  
  if (nrow(ent_sub) == 0) {
    cat("✗ Enterprise subscription tier not found\n")
    return(invisible(NULL))
  }
  
  sub_level_id      <- ent_sub$subscription_level_id[1]
  base_competitors  <- ent_sub$num_competitors_included[1]
  base_prompts      <- ent_sub$num_prompts_included[1]
  base_personas     <- ent_sub$num_personas_included[1]
  
  extra_competitors <- max(0, n_competitors - base_competitors)
  extra_prompts     <- max(0, n_prompts     - base_prompts)
  extra_personas    <- max(0, n_personas    - base_personas)
  
  # Close any existing subscriptions
  dbExecute(pool,
            "UPDATE fact_user_sub_level
     SET date_valid_to = CURRENT_DATE - 1
     WHERE login_id = $1 AND date_valid_to >= CURRENT_DATE",
            params = list(linked_login_id))
  
  # Insert subscription with extras
  dbExecute(pool,
            "INSERT INTO fact_user_sub_level
       (login_id, subscription_level_id, date_valid_from, date_valid_to,
        extra_competitors_added, extra_prompts_added, extra_personas_added)
     VALUES ($1, $2, CURRENT_DATE, '2099-12-31', $3, $4, $5)
     ON CONFLICT DO NOTHING",
            params = list(linked_login_id, sub_level_id,
                          extra_competitors, extra_prompts, extra_personas))
  
  cat(sprintf("✓ Subscription set: Enterprise + %d extra competitors, ",
              extra_competitors))
  cat(sprintf("%d extra prompts, %d extra personas\n",
              extra_prompts, extra_personas))
  
  # ── Summary ───────────────────────────────────────────────────────────
  cat("\n=== Linked account created ===\n")
  cat(sprintf("  Primary account:  %s (login_id: %d)\n",
              primary_email, primary_login_id))
  cat(sprintf("  Linked email:     %s\n", linked_email))
  cat(sprintf("  Linked login_id:  %d\n", linked_login_id))
  cat(sprintf("  Password:         %s\n", password))
  cat(sprintf("  Competitors:      %d (%d base + %d extra)\n",
              n_competitors, base_competitors, extra_competitors))
  cat(sprintf("  Prompts:          %d (%d base + %d extra)\n",
              n_prompts, base_prompts, extra_prompts))
  cat(sprintf("  Personas:         %d (%d base + %d extra)\n",
              n_personas, base_personas, extra_personas))
  cat("\nUser completes brand setup on first login.\n")
  
  return(invisible(list(
    primary_login_id = primary_login_id,
    primary_email    = primary_email,
    linked_login_id  = linked_login_id,
    linked_email     = linked_email,
    password         = password,
    n_competitors    = n_competitors,
    n_prompts        = n_prompts,
    n_personas       = n_personas
  )))
}

#' Generate onboarding demo report and email it
#'
#' @param login_id    Login ID of the demo account
#' @param brand_name  Brand name for the report title
#' @param to_email    Email address to send report to
generate_and_email_demo_report <- function(login_id,
                                           brand_name,
                                           to_email = c("alex@airrscore.com", "steven@airrscore.com")) {
  
  message("=== Generating demo report ===")
  message(sprintf("  login_id:   %d", login_id))
  message(sprintf("  brand_name: %s", brand_name))
  message(sprintf("  to_email:   %s", paste(to_email, collapse = ", ")))
  
  # ── Locate the Rmd template ────────────────────────────────────────
  # Try both web server and dev paths
  possible_rmd <- c(
    "/srv/shiny-server/AiRR/reports/onboarding_demo_report.Rmd",
    "/home/aarogers/airr/reports/onboarding_demo_report.Rmd",
    "/home/aarogers/AiRR/reports/onboarding_demo_report.Rmd"
  )
  
  rmd_path <- Find(file.exists, possible_rmd)
  
  if (is.null(rmd_path)) {
    message("✗ Report template not found")
    return(invisible(FALSE))
  }
  
  # ── Render to HTML ─────────────────────────────────────────────────
  output_dir  <- tempdir()
  output_file <- file.path(
    output_dir,
    sprintf("airr_demo_%s_%s.html",
            gsub("[^a-zA-Z0-9]", "_", brand_name),
            format(Sys.Date(), "%Y%m%d"))
  )
  
  message("  Rendering report...")
  
  render_result <- tryCatch({
    rmarkdown::render(
      input         = rmd_path,
      output_file   = output_file,
      output_format = "html_document",
      params        = list(
        login_id   = login_id,
        brand_name = brand_name,
        email      = to_email
      ),
      envir  = new.env(parent = globalenv()),
      quiet  = TRUE
    )
    TRUE
  }, error = function(e) {
    message(sprintf("✗ Render failed: %s", e$message))
    FALSE
  })
  
  if (!render_result || !file.exists(output_file)) {
    message("✗ Report file not created")
    return(invisible(FALSE))
  }
  
  file_size <- file.info(output_file)$size
  message(sprintf("  ✓ Report rendered: %.1f KB", file_size / 1024))
  
  # ── Send email ─────────────────────────────────────────────────────
  message("  Sending email...")
  
  subject <- sprintf("AiRR Demo Report — %s — %s",
                     brand_name,
                     format(Sys.Date(), "%B %d, %Y"))
  
  body <- paste0(
    "Hi,\n\n",
    "Here is the AiRR demo session report for ", brand_name, ".\n\n",
    "The report includes:\n",
    "  - AiRR Score and 4P breakdown\n",
    "  - Brand leaderboard vs competitors\n",
    "  - Score trends over time\n",
    "  - Prompt-level scores\n",
    "  - Customer persona scores\n\n",
    "The demo account has been reset and is ready for the next session.\n\n",
    "The AiRR Team\n",
    "airrscore.com"
  )
  
  smtp_host     <- Sys.getenv("SMTP_HOST")
  smtp_port     <- as.integer(Sys.getenv("SMTP_PORT") %||% "587")
  smtp_user     <- Sys.getenv("SMTP_USER")
  smtp_password <- Sys.getenv("SMTP_PASSWORD")
  from_email    <- Sys.getenv("SMTP_FROM") %||% "noreply@airrscore.com"
  
  email_result <- tryCatch({
    
    if (!nzchar(smtp_host)) {
      message(sprintf("  SMTP not configured — report saved to: %s",
                      output_file))
      return(invisible(TRUE))
    }
    
    # Build raw email with attachment using curl
    boundary   <- paste0("boundary_", format(Sys.time(), "%Y%m%d%H%M%S"))
    html_b64   <- base64enc::base64encode(output_file)
    attach_name <- basename(output_file)
    to_header <- paste(to_email, collapse = ", ")
    
    raw_email <- paste0(
      "From: AiRR <", from_email, ">\r\n",
      "To: ", to_header, "\r\n",
      "Subject: ", subject, "\r\n",
      "MIME-Version: 1.0\r\n",
      "Content-Type: multipart/mixed; boundary=\"", boundary, "\"\r\n",
      "\r\n",
      "--", boundary, "\r\n",
      "Content-Type: text/plain; charset=utf-8\r\n",
      "\r\n",
      body, "\r\n",
      "\r\n",
      "--", boundary, "\r\n",
      "Content-Type: text/html; name=\"", attach_name, "\"\r\n",
      "Content-Disposition: attachment; filename=\"", attach_name, "\"\r\n",
      "Content-Transfer-Encoding: base64\r\n",
      "\r\n",
      html_b64, "\r\n",
      "--", boundary, "--\r\n"
    )
    
    tmp_email <- tempfile()
    writeLines(raw_email, tmp_email)
    on.exit(unlink(tmp_email), add = TRUE)
    
    # Build curl args based on port
    # Port 465 = implicit SSL (smtps://)
    # Port 587 = STARTTLS (smtp:// + --ssl)
    smtp_url  <- if (smtp_port == 465) {
      paste0("smtps://", smtp_host, ":", smtp_port)
    } else {
      paste0("smtp://", smtp_host, ":", smtp_port)
    }
    
    ssl_args <- if (smtp_port == 465) {
      c("--ssl-reqd")   # implicit SSL for port 465
    } else {
      c("--ssl")        # STARTTLS for port 587
    }
    
    rcpt_args <- as.vector(rbind("--mail-rcpt", to_email))
    
    result <- system2(
      "curl",
      args = c(
        "--url",       smtp_url,
        ssl_args,
        "--user",      paste0(smtp_user, ":", smtp_password),
        "--mail-from", from_email,
        rcpt_args,
        "--upload-file", tmp_email,
        "--verbose"    # remove this once working
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    
    message("curl exit: ", attr(result, "status") %||% "0")
    message(paste(result, collapse = "\n"))
    
    message(sprintf("  ✓ Email sent to %s", paste(to_email, collapse = ", ")))
    TRUE
    
  }, error = function(e) {
    message(sprintf("  ✗ Email failed: %s", e$message))
    message(sprintf("  Report saved at: %s", output_file))
    FALSE
  })
  
  # Clean up temp file
  unlink(output_file)
  
  return(invisible(email_result))
}
