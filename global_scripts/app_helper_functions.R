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
    
    # Update fact_user_brands_tracked for all active brands
    dbExecute(pool, "
      UPDATE fact_user_brands_tracked
      SET brand_reach = $1
      WHERE login_id = $2
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
              params = list(brand_reach, login_id))
    
    # Update dim_brand for all brands this user tracks
    brand_ids <- dbGetQuery(pool, "
      SELECT DISTINCT brand_id
      FROM fact_user_brands_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
                            params = list(login_id))$brand_id
    
    if (length(brand_ids) > 0) {
      placeholders <- paste0("$", seq_along(brand_ids) + 4, collapse = ", ")
      dbExecute(pool,
                sprintf("UPDATE dim_brand 
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
    }
    
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
  sub <- get_user_subscription(login_id)
  if (sub$subscription_name != "Enterprise") {
    return(list(success = FALSE, message = "API access is an Enterprise feature."))
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

