resp <- request("https://api.openai.com/v1/chat/completions") |>
  req_headers(
    Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
    `Content-Type` = "application/json"
  ) |>
  req_body_json(list(
    model = "gpt-4o-mini",
    messages = list(list(role = "user", content = "test")),
    temperature = 0.1
  )) |>
  req_perform()


packageVersion("httr2")
args(req_perform_parallel)
t0 <- Sys.time()
result <- ask_chatgpt_async(rep("Say hello", 20), model = "gpt-4o-mini", temperature = 0.1)
cat(sprintf("20 requests in %.1fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))


responses <- req_perform_parallel(
  batch_requests,
  on_error = "continue",
  pool = curl::new_pool(total_con = 80, host_con = 80)
)


limits <- check_rate_limits("gpt-4.1-mini")
cat("RPM limit:", limits$requests_limit, "\n")
cat("TPM limit:", limits$tokens_limit, "\n")

query <- "
    select a.*,
    b.query_string,
    c.brand_name
    from fact_query_history as a
     left join
      dim_query as b
        on a.query_id = b.query_id
     left join
      dim_brand as c
        on a.brand_id = c.brand_id
  "

result <- dbGetQuery(pool, query)

write.csv(result,'prompt_results.csv')


recalculate_all_persistence_and_airr <- function() {
  
  message("=== Recalculating all historical persistence and AIRR scores ===\n")
  
  # ============================================
  # PART 1: Main brand scores (fact tables)
  # ============================================
  
  message("--- PART 1: Main brand scores ---\n")
  
  all_brands <- dbGetQuery(con, "SELECT brand_id, brand_name FROM dim_brand ORDER BY brand_id")
  
  failed_brands <- c()
  
  for (i in seq_len(nrow(all_brands))) {
    bid <- all_brands$brand_id[i]
    bname <- all_brands$brand_name[i]
    
    tryCatch({
      message(sprintf("[%d/%d] Processing %s (ID: %d)...", 
                      i, nrow(all_brands), bname, bid))
      
      # Get all dates where this brand has presence scores
      presence_dates <- dbGetQuery(con, "
        SELECT date, overall_score 
        FROM fact_presence_history 
        WHERE brand_id = $1 
        ORDER BY date",
                                   params = list(bid))
      
      if (nrow(presence_dates) == 0) {
        message(sprintf("  Skipping %s - no presence data", bname))
        next
      }
      
      # Recalculate persistence for each date
      for (d in seq_len(nrow(presence_dates))) {
        current_date <- presence_dates$date[d]
        
        # Get all presence scores up to this date
        history_up_to_date <- presence_dates[presence_dates$date <= current_date, ]
        
        if (nrow(history_up_to_date) < 5) {
          # Not enough data - persistence = latest presence score
          latest_score <- tail(history_up_to_date$overall_score, 1)
          new_persistence <- latest_score
          new_cv <- 0
          new_interpretation <- "Not enough data"
          new_daily_change <- 0
        } else {
          # Calculate using the updated formula
          values <- history_up_to_date$overall_score
          latest_score <- tail(values, 1)
          
          cv <- calculate_coefficient_of_variation(values)
          
          if (!is.null(cv)) {
            new_persistence <- max(0, (1 - cv) * latest_score)
            new_cv <- round(cv, 4)
          } else {
            new_persistence <- latest_score
            new_cv <- 0
          }
          
          stability_ratio <- if (latest_score > 0) new_persistence / latest_score else 0
          
          new_interpretation <- dplyr::case_when(
            stability_ratio >= 0.85 ~ "Excellent - Highly stable and consistent presence",
            stability_ratio >= 0.70 ~ "Good - Stable presence with minor variations",
            stability_ratio >= 0.55 ~ "Fair - Moderate stability",
            stability_ratio >= 0.40 ~ "Poor - Significant fluctuations",
            TRUE ~ "Very Poor - Highly unstable presence"
          )
          
          # Daily change
          if (nrow(history_up_to_date) >= 2) {
            prev <- tail(head(values, -1), 1)
            if (prev != 0) {
              new_daily_change <- 100 * (latest_score - prev) / prev
            } else {
              new_daily_change <- 0
            }
          } else {
            new_daily_change <- 0
          }
        }
        
        new_persistence <- round(new_persistence, 2)
        
        # Update persistence
        dbExecute(con, "
          UPDATE fact_persistence_history
          SET persistence_score = $1,
              coefficient_of_variation = $2,
              interpretation = $3,
              daily_perc_change = $4
          WHERE brand_id = $5 AND date = $6",
                  params = list(
                    new_persistence, new_cv, new_interpretation,
                    as.character(round(new_daily_change, 4)),
                    bid, current_date
                  ))
      }
      
      message(sprintf("  ✓ Updated %d persistence scores for %s", 
                      nrow(presence_dates), bname))
      
      # Now recalculate AIRR for each date
      airr_dates <- dbGetQuery(con, "
        SELECT date FROM fact_airr_history 
        WHERE brand_id = $1 ORDER BY date",
                               params = list(bid))
      
      for (d in seq_len(nrow(airr_dates))) {
        current_date <- airr_dates$date[d]
        
        presence <- dbGetQuery(con,
                               "SELECT overall_score FROM fact_presence_history 
           WHERE brand_id = $1 AND date = $2",
                               params = list(bid, current_date))$overall_score
        
        perception <- dbGetQuery(con,
                                 "SELECT perception_score FROM fact_perception_history 
           WHERE brand_id = $1 AND date = $2",
                                 params = list(bid, current_date))$perception_score
        
        prestige <- dbGetQuery(con,
                               "SELECT prestige_score FROM fact_prestige_history 
           WHERE brand_id = $1 AND date = $2",
                               params = list(bid, current_date))$prestige_score
        
        persistence <- dbGetQuery(con,
                                  "SELECT persistence_score FROM fact_persistence_history 
           WHERE brand_id = $1 AND date = $2",
                                  params = list(bid, current_date))$persistence_score
        
        # Skip if any component missing
        if (length(presence) == 0 || length(perception) == 0 || 
            length(prestige) == 0 || length(persistence) == 0) {
          next
        }
        
        new_airr <- presence * AIRR_WEIGHTS$presence +
          perception * AIRR_WEIGHTS$perception +
          prestige * AIRR_WEIGHTS$prestige +
          persistence * AIRR_WEIGHTS$persistence
        
        dbExecute(con, "
          UPDATE fact_airr_history
          SET airr_score = $1
          WHERE brand_id = $2 AND date = $3",
                  params = list(new_airr, bid, current_date))
      }
      
      message(sprintf("  ✓ Updated %d AIRR scores for %s", 
                      nrow(airr_dates), bname))
      
    }, error = function(e) {
      failed_brands <<- c(failed_brands, bname)
      message(sprintf("  ✗ %s failed: %s", bname, e$message))
    })
  }
  
  # ============================================
  # PART 2: Query/prompt scores (fact_query_history)
  # ============================================
  
  message("\n--- PART 2: Query/prompt scores ---\n")
  
  all_query_combos <- dbGetQuery(con, "
    SELECT DISTINCT brand_id, query_id 
    FROM fact_query_history 
    ORDER BY brand_id, query_id")
  
  failed_queries <- c()
  
  for (row in seq_len(nrow(all_query_combos))) {
    bid <- all_query_combos$brand_id[row]
    qid <- all_query_combos$query_id[row]
    
    tryCatch({
      # Get brand name for logging
      bname <- dbGetQuery(con, 
                          "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                          params = list(bid))$brand_name
      
      message(sprintf("[%d/%d] Brand: %s, Query: %d...", 
                      row, nrow(all_query_combos), bname, qid))
      
      # Get all dates for this brand/query combo
      query_dates <- dbGetQuery(con, "
        SELECT date, presence_score 
        FROM fact_query_history 
        WHERE brand_id = $1 AND query_id = $2 
        ORDER BY date",
                                params = list(bid, qid))
      
      if (nrow(query_dates) == 0) next
      
      for (d in seq_len(nrow(query_dates))) {
        current_date <- query_dates$date[d]
        
        # Get presence history up to this date for this query
        history_up_to_date <- query_dates[query_dates$date <= current_date, ]
        
        if (nrow(history_up_to_date) < 5) {
          # Not enough data - persistence = latest presence score
          new_persistence <- tail(history_up_to_date$presence_score, 1)
        } else {
          values <- history_up_to_date$presence_score
          latest_score <- tail(values, 1)
          
          cv <- calculate_coefficient_of_variation(values)
          
          if (!is.null(cv)) {
            new_persistence <- max(0, (1 - cv) * latest_score)
          } else {
            new_persistence <- latest_score
          }
        }
        
        new_persistence <- round(new_persistence, 2)
        
        # Get today's other scores
        todays <- dbGetQuery(con, "
          SELECT presence_score, perception_score, prestige_score 
          FROM fact_query_history 
          WHERE brand_id = $1 AND query_id = $2 AND date = $3",
                             params = list(bid, qid, current_date))
        
        if (nrow(todays) == 0) next
        
        new_airr <- todays$presence_score * AIRR_WEIGHTS$presence +
          todays$perception_score * AIRR_WEIGHTS$perception +
          todays$prestige_score * AIRR_WEIGHTS$prestige +
          new_persistence * AIRR_WEIGHTS$persistence
        
        # Update both persistence and airr
        dbExecute(con, "
          UPDATE fact_query_history
          SET persistence_score = $1, airr_score = $2
          WHERE brand_id = $3 AND query_id = $4 AND date = $5",
                  params = list(new_persistence, new_airr, bid, qid, current_date))
      }
      
      message(sprintf("  ✓ Updated %d dates for %s / query %d", 
                      nrow(query_dates), bname, qid))
      
    }, error = function(e) {
      failed_queries <<- c(failed_queries, paste0("brand:", bid, "/query:", qid))
      message(sprintf("  ✗ Brand %d, Query %d failed: %s", bid, qid, e$message))
    })
  }
  
  # ============================================
  # Summary
  # ============================================
  
  message("\n=== RECALCULATION COMPLETE ===")
  message(sprintf("Brands processed: %d", nrow(all_brands)))
  message(sprintf("Query combos processed: %d", nrow(all_query_combos)))
  
  if (length(failed_brands) > 0) {
    message(sprintf("Failed brands: %s", paste(failed_brands, collapse = ", ")))
  }
  if (length(failed_queries) > 0) {
    message(sprintf("Failed queries: %s", paste(failed_queries, collapse = ", ")))
  }
  if (length(failed_brands) == 0 && length(failed_queries) == 0) {
    message("✓ All recalculations successful!")
  }
}

# Source the updated persistence.R first to get the new formula
source("global_scripts/persistence.R")

# Then run the recalculation
recalculate_all_persistence_and_airr()






recalculate_last_two_days <- function() {
  
  message("=== Recalculating persistence & AIRR for last 2 days ===\n")
  
  cutoff_date <- Sys.Date() - 1  # yesterday and today
  
  # ============================================
  # PART 1: Main brand scores
  # ============================================
  
  message("--- PART 1: Main brand scores ---\n")
  
  # Get all brand/date combos from last 2 days
  brand_dates <- dbGetQuery(con, "
    SELECT DISTINCT ph.brand_id, db.brand_name, ph.date
    FROM fact_presence_history ph
    JOIN dim_brand db ON db.brand_id = ph.brand_id
    WHERE ph.date >= $1
    ORDER BY ph.brand_id, ph.date",
                            params = list(cutoff_date))
  
  message(sprintf("Found %d brand/date combos to update\n", nrow(brand_dates)))
  
  failed <- c()
  
  for (r in seq_len(nrow(brand_dates))) {
    bid <- brand_dates$brand_id[r]
    bname <- brand_dates$brand_name[r]
    current_date <- brand_dates$date[r]
    
    tryCatch({
      message(sprintf("[%d/%d] %s — %s", r, nrow(brand_dates), bname, current_date))
      
      # Get all presence history up to this date
      history <- dbGetQuery(con, "
        SELECT date, overall_score 
        FROM fact_presence_history 
        WHERE brand_id = $1 AND date <= $2
        ORDER BY date",
                            params = list(bid, current_date))
      
      if (nrow(history) == 0) next
      
      if (nrow(history) < 5) {
        latest_score <- tail(history$overall_score, 1)
        new_persistence <- latest_score
        new_cv <- 0
        new_interpretation <- "Not enough data"
        new_daily_change <- 0
      } else {
        values <- history$overall_score
        latest_score <- tail(values, 1)
        
        cv <- calculate_coefficient_of_variation(values)
        
        if (!is.null(cv)) {
          new_persistence <- max(0, (1 - cv) * latest_score)
          new_cv <- round(cv, 4)
        } else {
          new_persistence <- latest_score
          new_cv <- 0
        }
        
        stability_ratio <- if (latest_score > 0) new_persistence / latest_score else 0
        
        new_interpretation <- dplyr::case_when(
          stability_ratio >= 0.85 ~ "Excellent - Highly stable and consistent presence",
          stability_ratio >= 0.70 ~ "Good - Stable presence with minor variations",
          stability_ratio >= 0.55 ~ "Fair - Moderate stability",
          stability_ratio >= 0.40 ~ "Poor - Significant fluctuations",
          TRUE ~ "Very Poor - Highly unstable presence"
        )
        
        if (nrow(history) >= 2) {
          prev <- tail(head(values, -1), 1)
          new_daily_change <- if (prev != 0) 100 * (latest_score - prev) / prev else 0
        } else {
          new_daily_change <- 0
        }
      }
      
      new_persistence <- round(new_persistence, 2)
      
      # Update persistence
      dbExecute(con, "
        UPDATE fact_persistence_history
        SET persistence_score = $1,
            coefficient_of_variation = $2,
            interpretation = $3,
            daily_perc_change = $4
        WHERE brand_id = $5 AND date = $6",
                params = list(
                  new_persistence, new_cv, new_interpretation,
                  as.character(round(new_daily_change, 4)),
                  bid, current_date
                ))
      
      # Recalculate AIRR
      presence <- dbGetQuery(con,
                             "SELECT overall_score FROM fact_presence_history 
         WHERE brand_id = $1 AND date = $2",
                             params = list(bid, current_date))$overall_score
      
      perception <- dbGetQuery(con,
                               "SELECT perception_score FROM fact_perception_history 
         WHERE brand_id = $1 AND date = $2",
                               params = list(bid, current_date))$perception_score
      
      prestige <- dbGetQuery(con,
                             "SELECT prestige_score FROM fact_prestige_history 
         WHERE brand_id = $1 AND date = $2",
                             params = list(bid, current_date))$prestige_score
      
      if (length(presence) > 0 && length(perception) > 0 && 
          length(prestige) > 0) {
        new_airr <- presence * AIRR_WEIGHTS$presence +
          perception * AIRR_WEIGHTS$perception +
          prestige * AIRR_WEIGHTS$prestige +
          new_persistence * AIRR_WEIGHTS$persistence
        
        dbExecute(con, "
          UPDATE fact_airr_history
          SET airr_score = $1
          WHERE brand_id = $2 AND date = $3",
                  params = list(new_airr, bid, current_date))
      }
      
      message(sprintf("  ✓ persistence=%.2f, cv=%.4f", new_persistence, new_cv))
      
    }, error = function(e) {
      failed <<- c(failed, paste0(bname, "/", current_date))
      message(sprintf("  ✗ Failed: %s", e$message))
    })
  }
  
  # ============================================
  # PART 2: Query/prompt scores
  # ============================================
  
  message("\n--- PART 2: Query/prompt scores ---\n")
  
  query_dates <- dbGetQuery(con, "
    SELECT DISTINCT fqh.brand_id, db.brand_name, fqh.query_id, dq.query_string, fqh.date
    FROM fact_query_history fqh
    JOIN dim_brand db ON db.brand_id = fqh.brand_id
    JOIN dim_query dq ON dq.query_id = fqh.query_id
    WHERE fqh.date >= $1
    ORDER BY fqh.brand_id, fqh.query_id, fqh.date",
                            params = list(cutoff_date))
  
  message(sprintf("Found %d query/brand/date combos to update\n", nrow(query_dates)))
  
  failed_queries <- c()
  
  for (r in seq_len(nrow(query_dates))) {
    bid <- query_dates$brand_id[r]
    bname <- query_dates$brand_name[r]
    qid <- query_dates$query_id[r]
    current_date <- query_dates$date[r]
    
    tryCatch({
      message(sprintf("[%d/%d] %s / query %d — %s", 
                      r, nrow(query_dates), bname, qid, current_date))
      
      # Get presence history for this brand/query up to this date
      history <- dbGetQuery(con, "
        SELECT date, presence_score 
        FROM fact_query_history 
        WHERE brand_id = $1 AND query_id = $2 AND date <= $3
        ORDER BY date",
                            params = list(bid, qid, current_date))
      
      if (nrow(history) == 0) next
      
      if (nrow(history) < 5) {
        new_persistence <- tail(history$presence_score, 1)
      } else {
        values <- history$presence_score
        latest_score <- tail(values, 1)
        
        cv <- calculate_coefficient_of_variation(values)
        
        if (!is.null(cv)) {
          new_persistence <- max(0, (1 - cv) * latest_score)
        } else {
          new_persistence <- latest_score
        }
      }
      
      new_persistence <- round(new_persistence, 2)
      
      # Get today's other scores
      todays <- dbGetQuery(con, "
        SELECT presence_score, perception_score, prestige_score 
        FROM fact_query_history 
        WHERE brand_id = $1 AND query_id = $2 AND date = $3",
                           params = list(bid, qid, current_date))
      
      if (nrow(todays) == 0) next
      
      new_airr <- todays$presence_score * AIRR_WEIGHTS$presence +
        todays$perception_score * AIRR_WEIGHTS$perception +
        todays$prestige_score * AIRR_WEIGHTS$prestige +
        new_persistence * AIRR_WEIGHTS$persistence
      
      dbExecute(con, "
        UPDATE fact_query_history
        SET persistence_score = $1, airr_score = $2
        WHERE brand_id = $3 AND query_id = $4 AND date = $5",
                params = list(new_persistence, round(new_airr, 2), bid, qid, current_date))
      
      message(sprintf("  ✓ persistence=%.2f, airr=%.2f", new_persistence, round(new_airr, 2)))
      
    }, error = function(e) {
      failed_queries <<- c(failed_queries, paste0(bname, "/q", qid, "/", current_date))
      message(sprintf("  ✗ Failed: %s", e$message))
    })
  }
  
  # ============================================
  # Summary
  # ============================================
  
  message("\n=== COMPLETE ===")
  message(sprintf("Brand scores updated: %d", nrow(brand_dates)))
  message(sprintf("Query scores updated: %d", nrow(query_dates)))
  
  if (length(failed) > 0) {
    message(sprintf("Failed brands: %s", paste(failed, collapse = ", ")))
  }
  if (length(failed_queries) > 0) {
    message(sprintf("Failed queries: %s", paste(failed_queries, collapse = ", ")))
  }
  if (length(failed) == 0 && length(failed_queries) == 0) {
    message("✓ All recalculations successful!")
  }
}

# Run it
recalculate_last_two_days()




# ============================================
# Step 1: Get the login_ids
# ============================================

nike_user <- dbGetQuery(pool, "
  SELECT du.login_id, db.brand_id
  FROM dim_user du
  JOIN fact_user_brands_tracked ubt ON du.login_id = ubt.login_id
  JOIN dim_brand db ON ubt.brand_id = db.brand_id
  WHERE db.brand_name = 'Nike' AND ubt.main_brand_flag = TRUE
  LIMIT 1
")

ua_user <- dbGetQuery(pool, "
  SELECT du.login_id, db.brand_id
  FROM dim_user du
  JOIN fact_user_brands_tracked ubt ON du.login_id = ubt.login_id
  JOIN dim_brand db ON ubt.brand_id = db.brand_id
  WHERE db.brand_name = 'Under Armour' AND ubt.main_brand_flag = TRUE
  LIMIT 1
")

cat("Nike login_id:", nike_user$login_id, "brand_id:", nike_user$brand_id, "\n")
cat("UA login_id:", ua_user$login_id, "brand_id:", ua_user$brand_id, "\n")

# ============================================
# Step 2: Get Nike's tracked brands (excluding Under Armour)
# ============================================

nike_brands <- dbGetQuery(pool, "
  SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON ubt.brand_id = b.brand_id
  WHERE ubt.login_id = $1
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    AND b.brand_name != 'Under Armour'
", params = list(nike_user$login_id))

cat("\nNike's tracked brands (excluding Under Armour):\n")
print(nike_brands)

# ============================================
# Step 3: Add those brands for Under Armour user
# ============================================

for (i in 1:nrow(nike_brands)) {
  bid <- nike_brands$brand_id[i]
  bname <- nike_brands$brand_name[i]
  is_main <- nike_brands$main_brand_flag[i]
  
  # Skip if it's Nike (which would be a competitor for UA, not main)
  if (bname == "Nike") {
    is_main <- FALSE
  }
  
  # Skip UA's own main brand (already set up)
  if (bid == ua_user$brand_id) {
    cat("Skipping", bname, "- this is UA's main brand\n")
    next
  }
  
  result <- add_brand_for_user_pending(ua_user$login_id, bname, main_brand = is_main)
  
  if (!is.null(result)) {
    status <- if (result$needs_scoring) "needs scoring" else "scores exist"
    cat(sprintf("✓ Added %s (brand_id: %d) — %s\n", bname, result$brand_id, status))
    
    # Only score if needed
    if (result$needs_scoring) {
      cat(sprintf("  Calculating scores for %s...\n", bname))
      tryCatch({
        user_create_airr(bname)
        cat(sprintf("  ✓ Scores calculated for %s\n", bname))
      }, error = function(e) {
        cat(sprintf("  ✗ Scoring failed for %s: %s\n", bname, e$message))
      })
    }
  } else {
    cat(sprintf("✗ Failed to add %s\n", bname))
  }
}

# ============================================
# Step 4: Get Nike's tracked prompts
# ============================================

nike_prompts <- dbGetQuery(pool, "
  SELECT DISTINCT dq.query_id, dq.query_string
  FROM dim_brand_query dbq
  JOIN dim_query dq ON dbq.query_id = dq.query_id
  WHERE dbq.brand_id = $1
", params = list(nike_user$brand_id))

cat("\nNike's tracked prompts:\n")
print(nike_prompts)

# ============================================
# Step 5: Add those prompts for all UA's brands
# ============================================

# Get all of UA's brand_ids
ua_brands <- dbGetQuery(pool, "
  SELECT b.brand_id, b.brand_name
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON ubt.brand_id = b.brand_id
  WHERE ubt.login_id = $1
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
", params = list(ua_user$login_id))

cat("\nUA's brands (to link prompts to):\n")
print(ua_brands)

for (i in 1:nrow(nike_prompts)) {
  qid <- nike_prompts$query_id[i]
  qstring <- nike_prompts$query_string[i]
  
  cat(sprintf("\nProcessing prompt: '%s' (query_id: %d)\n", qstring, qid))
  
  for (j in 1:nrow(ua_brands)) {
    bid <- ua_brands$brand_id[j]
    bname <- ua_brands$brand_name[j]
    
    # Link prompt to brand
    tryCatch({
      dbExecute(pool, "
        INSERT INTO dim_brand_query (brand_id, query_id, date_added)
        VALUES ($1, $2, $3)
        ON CONFLICT (brand_id, query_id) DO NOTHING
      ", params = list(bid, qid, Sys.Date()))
      
      cat(sprintf("  ✓ Linked query %d to %s (brand_id: %d)\n", qid, bname, bid))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed to link query %d to %s: %s\n", qid, bname, e$message))
    })
  }
  
  # Check if scores already exist for UA's main brand + this query
  existing_scores <- dbGetQuery(pool, "
    SELECT COUNT(*) as cnt 
    FROM fact_query_history 
    WHERE brand_id = $1 AND query_id = $2
  ", params = list(ua_user$brand_id, qid))
  
  if (existing_scores$cnt == 0) {
    cat(sprintf("  Calculating prompt scores for query %d...\n", qid))
    tryCatch({
      create_prompt_airr(ua_user$brand_id, qstring, qid)
      cat(sprintf("  ✓ Prompt scores calculated for query %d\n", qid))
    }, error = function(e) {
      cat(sprintf("  ✗ Prompt scoring failed for query %d: %s\n", qid, e$message))
    })
  } else {
    cat(sprintf("  Scores already exist for query %d — skipping\n", qid))
  }
}

# ============================================
# Step 6: Verify
# ============================================

cat("\n\n=== VERIFICATION ===\n")

ua_final_brands <- get_user_brands(ua_user$login_id)
cat("\nUA's final brand list:\n")
print(ua_final_brands)

ua_final_prompts <- dbGetQuery(pool, "
  SELECT DISTINCT dq.query_id, dq.query_string
  FROM dim_brand_query dbq
  JOIN dim_query dq ON dbq.query_id = dq.query_id
  WHERE dbq.brand_id = $1
", params = list(ua_user$brand_id))
cat("\nUA's final prompt list:\n")
print(ua_final_prompts)

cat("\nDone!\n")



# Run this once — backfill_industry.R
# Set working directory and source dependencies first
# setwd('/srv/shiny-server/AiRR')
source("global_scripts/chatgpt_functions.R")
source("global_scripts/app_helper_functions.R")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = Sys.getenv("DB_PORT"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

brands_without_industry <- dbGetQuery(con, "
  SELECT brand_id, brand_name 
  FROM dim_brand 
  WHERE industry IS NULL OR industry = ''
  ORDER BY brand_name
")

message(sprintf("Backfilling industry for %d brands...", nrow(brands_without_industry)))

for (i in seq_len(nrow(brands_without_industry))) {
  brand_name <- brands_without_industry$brand_name[i]
  brand_id   <- brands_without_industry$brand_id[i]
  
  tryCatch({
    prompt <- paste0(
      "What industry is the brand '", brand_name, "' in? ",
      "Respond with ONLY the industry name, nothing else. ",
      "Examples: 'Athletic Footwear', 'Fast Food', 'Commercial Banking', ",
      "'Residential Real Estate', 'Enterprise Software'."
    )
    
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(
        model       = "gpt-4o-mini",
        messages    = list(list(role = "user", content = prompt)),
        temperature = 0.1,
        max_tokens  = 20
      )) |>
      req_perform()
    
    parsed   <- resp_body_json(resp)
    industry <- trimws(sanitise_text(parsed$choices[[1]]$message$content))
    
    dbExecute(con,
              "UPDATE dim_brand SET industry = $1 WHERE brand_id = $2",
              params = list(industry, brand_id))
    
    message(sprintf("  ✓ %s → %s", brand_name, industry))
    Sys.sleep(0.5)
    
  }, error = function(e) {
    message(sprintf("  ✗ %s failed: %s", brand_name, e$message))
  })
}

# dbDisconnect(con)
message("Done.")





# ---------------------------
# Check what we're working with
dbGetQuery(pool, "
  SELECT 
    COUNT(*) as total_rows,
    COUNT(login_id) as rows_with_login_id,
    COUNT(*) - COUNT(login_id) as rows_with_null_login_id
  FROM fact_airr_history
")

# Check if there are any rows that have NO matching user at all
# (these would be orphaned brand scores with no user tracking them)
dbGetQuery(pool, "
  SELECT COUNT(*) as orphaned_rows
  FROM fact_airr_history fah
  WHERE fah.login_id IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM fact_user_brands_tracked ubt
    WHERE ubt.brand_id = fah.brand_id
      AND ubt.date_valid_from <= fah.date
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= fah.date)
  )
")

# See which brands have null login_id scores and who tracks them
# Which brands are ENTIRELY orphaned (all their rows have no matching user)
# vs PARTIALLY orphaned (some rows match a user, some don't)
dbGetQuery(pool, "
  SELECT 
    db.brand_name,
    db.brand_id,
    COUNT(*) as total_score_rows,
    SUM(CASE WHEN NOT EXISTS (
      SELECT 1 FROM fact_user_brands_tracked ubt
      WHERE ubt.brand_id = fah.brand_id
        AND ubt.date_valid_from <= fah.date
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= fah.date)
    ) THEN 1 ELSE 0 END) as orphaned_rows,
    SUM(CASE WHEN EXISTS (
      SELECT 1 FROM fact_user_brands_tracked ubt
      WHERE ubt.brand_id = fah.brand_id
        AND ubt.date_valid_from <= fah.date
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= fah.date)
    ) THEN 1 ELSE 0 END) as matched_rows,
    CASE 
      WHEN SUM(CASE WHEN EXISTS (
        SELECT 1 FROM fact_user_brands_tracked ubt
        WHERE ubt.brand_id = fah.brand_id
          AND ubt.date_valid_from <= fah.date
          AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= fah.date)
      ) THEN 1 ELSE 0 END) = 0 
      THEN 'ENTIRELY ORPHANED'
      ELSE 'PARTIALLY ORPHANED'
    END as orphan_status
  FROM fact_airr_history fah
  JOIN dim_brand db ON db.brand_id = fah.brand_id
  WHERE NOT EXISTS (
    SELECT 1 FROM fact_user_brands_tracked ubt
    WHERE ubt.brand_id = fah.brand_id
      AND ubt.date_valid_from <= fah.date
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= fah.date)
  )
  OR EXISTS (
    SELECT 1 FROM fact_user_brands_tracked ubt
    WHERE ubt.brand_id = fah.brand_id
      AND ubt.date_valid_from <= fah.date
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= fah.date)
  )
  GROUP BY db.brand_name, db.brand_id
  HAVING SUM(CASE WHEN NOT EXISTS (
    SELECT 1 FROM fact_user_brands_tracked ubt
    WHERE ubt.brand_id = fah.brand_id
      AND ubt.date_valid_from <= fah.date
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= fah.date)
  ) THEN 1 ELSE 0 END) > 0
  ORDER BY orphan_status, db.brand_name
")

# ============================================
# STEP 1: Delete entirely orphaned brands' scores
# (Fila, Miller Light, Ro, Umbro - brand_ids 3, 26, 19, 5)
# ============================================

entirely_orphaned_ids <- c(3, 26, 19, 5)

dbExecute(pool, "
  DELETE FROM fact_airr_history 
  WHERE brand_id IN (3, 26, 19, 5)
")
cat("Deleted entirely orphaned airr rows\n")

dbExecute(pool, "
  DELETE FROM fact_presence_history 
  WHERE brand_id IN (3, 26, 19, 5)
")
cat("Deleted entirely orphaned presence rows\n")

dbExecute(pool, "
  DELETE FROM fact_perception_history 
  WHERE brand_id IN (3, 26, 19, 5)
")
cat("Deleted entirely orphaned perception rows\n")

dbExecute(pool, "
  DELETE FROM fact_prestige_history 
  WHERE brand_id IN (3, 26, 19, 5)
")
cat("Deleted entirely orphaned prestige rows\n")

dbExecute(pool, "
  DELETE FROM fact_persistence_history 
  WHERE brand_id IN (3, 26, 19, 5)
")
cat("Deleted entirely orphaned persistence rows\n")

# ============================================
# STEP 2: Expand partially orphaned rows
# For these, assign to ANY user who tracks that brand
# regardless of date window (timing mismatch fix)
# ============================================

# fact_airr_history
dbExecute(pool, "
  CREATE TEMP TABLE fact_airr_expanded AS
  SELECT DISTINCT
    fah.brand_id,
    ubt.login_id,
    fah.date,
    fah.airr_score
  FROM fact_airr_history fah
  JOIN (
    SELECT DISTINCT brand_id, login_id 
    FROM fact_user_brands_tracked
    WHERE date_valid_from IS NOT NULL
  ) ubt ON ubt.brand_id = fah.brand_id
  WHERE fah.login_id IS NULL
")

dbGetQuery(pool, "SELECT COUNT(*) as rows_to_insert FROM fact_airr_expanded")

dbExecute(pool, "DELETE FROM fact_airr_history WHERE login_id IS NULL")

dbExecute(pool, "
  INSERT INTO fact_airr_history (brand_id, login_id, date, airr_score)
  SELECT brand_id, login_id, date, airr_score
  FROM fact_airr_expanded
")
cat("fact_airr_history expansion done\n")

# fact_presence_history
dbExecute(pool, "
  CREATE TEMP TABLE fact_presence_expanded AS
  SELECT DISTINCT
    fph.brand_id,
    ubt.login_id,
    fph.date,
    fph.overall_score,
    fph.simple_mention_rate,
    fph.total_responses,
    fph.responses_with_mentions,
    fph.interpretation,
    fph.stability
  FROM fact_presence_history fph
  JOIN (
    SELECT DISTINCT brand_id, login_id 
    FROM fact_user_brands_tracked
    WHERE date_valid_from IS NOT NULL
  ) ubt ON ubt.brand_id = fph.brand_id
  WHERE fph.login_id IS NULL
")

dbExecute(pool, "DELETE FROM fact_presence_history WHERE login_id IS NULL")

# ============================================
# Fix fact_presence_history PK first, then insert
# ============================================

dbExecute(pool, "
  ALTER TABLE fact_presence_history
  DROP CONSTRAINT IF EXISTS fact_presence_history_pkey
")

dbExecute(pool, "
  ALTER TABLE fact_presence_history
  ADD CONSTRAINT fact_presence_history_pkey
  PRIMARY KEY (brand_id, login_id, date)
")
cat("fact_presence_history PK updated\n")

dbExecute(pool, "
  INSERT INTO fact_presence_history (
    brand_id, login_id, date, overall_score, simple_mention_rate,
    total_responses, responses_with_mentions, interpretation, stability
  )
  SELECT brand_id, login_id, date, overall_score, simple_mention_rate,
    total_responses, responses_with_mentions, interpretation, stability
  FROM fact_presence_expanded
")
cat("fact_presence_history expansion done\n")

# ============================================
# fact_perception_history — PK first then insert
# ============================================

dbExecute(pool, "
  CREATE TEMP TABLE fact_perception_expanded AS
  SELECT DISTINCT
    fph.brand_id,
    ubt.login_id,
    fph.date,
    fph.perception_score,
    fph.perception_accuracy_score,
    fph.perception_sentiment_score,
    fph.prestige_accuracy_interpretation
  FROM fact_perception_history fph
  JOIN (
    SELECT DISTINCT brand_id, login_id 
    FROM fact_user_brands_tracked
    WHERE date_valid_from IS NOT NULL
  ) ubt ON ubt.brand_id = fph.brand_id
  WHERE fph.login_id IS NULL
")

dbExecute(pool, "DELETE FROM fact_perception_history WHERE login_id IS NULL")

dbExecute(pool, "
  ALTER TABLE fact_perception_history
  DROP CONSTRAINT IF EXISTS fact_perception_history_pkey
")

dbExecute(pool, "
  ALTER TABLE fact_perception_history
  ADD CONSTRAINT fact_perception_history_pkey
  PRIMARY KEY (brand_id, login_id, date)
")
cat("fact_perception_history PK updated\n")

dbExecute(pool, "
  INSERT INTO fact_perception_history (
    brand_id, login_id, date, perception_score, perception_accuracy_score,
    perception_sentiment_score, prestige_accuracy_interpretation
  )
  SELECT brand_id, login_id, date, perception_score, perception_accuracy_score,
    perception_sentiment_score, prestige_accuracy_interpretation
  FROM fact_perception_expanded
")
cat("fact_perception_history expansion done\n")

# ============================================
# fact_prestige_history — PK first then insert
# ============================================

dbExecute(pool, "
  CREATE TEMP TABLE fact_prestige_expanded AS
  SELECT DISTINCT
    fph.brand_id,
    ubt.login_id,
    fph.date,
    fph.prestige_score,
    fph.prestige_rank_score,
    fph.prestige_rank_comps_brands,
    fph.prestige_authority_score,
    fph.prestige_leadership_score
  FROM fact_prestige_history fph
  JOIN (
    SELECT DISTINCT brand_id, login_id 
    FROM fact_user_brands_tracked
    WHERE date_valid_from IS NOT NULL
  ) ubt ON ubt.brand_id = fph.brand_id
  WHERE fph.login_id IS NULL
")

dbExecute(pool, "DELETE FROM fact_prestige_history WHERE login_id IS NULL")

dbExecute(pool, "
  ALTER TABLE fact_prestige_history
  DROP CONSTRAINT IF EXISTS fact_prestige_history_pkey
")

dbExecute(pool, "
  ALTER TABLE fact_prestige_history
  ADD CONSTRAINT fact_prestige_history_pkey
  PRIMARY KEY (brand_id, login_id, date)
")
cat("fact_prestige_history PK updated\n")

dbExecute(pool, "
  INSERT INTO fact_prestige_history (
    brand_id, login_id, date, prestige_score, prestige_rank_score,
    prestige_rank_comps_brands, prestige_authority_score, prestige_leadership_score
  )
  SELECT brand_id, login_id, date, prestige_score, prestige_rank_score,
    prestige_rank_comps_brands, prestige_authority_score, prestige_leadership_score
  FROM fact_prestige_expanded
")
cat("fact_prestige_history expansion done\n")

# ============================================
# fact_persistence_history — PK first then insert
# ============================================

dbExecute(pool, "
  CREATE TEMP TABLE fact_persistence_expanded AS
  SELECT DISTINCT
    fph.brand_id,
    ubt.login_id,
    fph.date,
    fph.persistence_score,
    fph.coefficient_of_variation,
    fph.interpretation,
    fph.daily_perc_change
  FROM fact_persistence_history fph
  JOIN (
    SELECT DISTINCT brand_id, login_id 
    FROM fact_user_brands_tracked
    WHERE date_valid_from IS NOT NULL
  ) ubt ON ubt.brand_id = fph.brand_id
  WHERE fph.login_id IS NULL
")

dbExecute(pool, "DELETE FROM fact_persistence_history WHERE login_id IS NULL")

dbExecute(pool, "
  ALTER TABLE fact_persistence_history
  DROP CONSTRAINT IF EXISTS fact_persistence_history_pkey
")

dbExecute(pool, "
  ALTER TABLE fact_persistence_history
  ADD CONSTRAINT fact_persistence_history_pkey
  PRIMARY KEY (brand_id, login_id, date)
")
cat("fact_persistence_history PK updated\n")

dbExecute(pool, "
  INSERT INTO fact_persistence_history (
    brand_id, login_id, date, persistence_score, coefficient_of_variation,
    interpretation, daily_perc_change
  )
  SELECT brand_id, login_id, date, persistence_score, coefficient_of_variation,
    interpretation, daily_perc_change
  FROM fact_persistence_expanded
")
cat("fact_persistence_history expansion done\n")

# ============================================
# Backfill industry
# ============================================

dbExecute(pool, "
  UPDATE fact_user_brands_tracked ubt
  SET industry = db.industry
  FROM dim_brand db
  WHERE ubt.brand_id = db.brand_id
    AND ubt.industry IS NULL
    AND db.industry IS NOT NULL
")
cat("industry backfill done\n")

# ============================================
# Final verification
# ============================================

verification <- dbGetQuery(pool, "
  SELECT 'fact_airr_history' as table_name,
    COUNT(*) as total_rows,
    COUNT(login_id) as rows_with_login_id,
    COUNT(*) - COUNT(login_id) as rows_still_null
  FROM fact_airr_history
  UNION ALL
  SELECT 'fact_presence_history',
    COUNT(*), COUNT(login_id), COUNT(*) - COUNT(login_id)
  FROM fact_presence_history
  UNION ALL
  SELECT 'fact_perception_history',
    COUNT(*), COUNT(login_id), COUNT(*) - COUNT(login_id)
  FROM fact_perception_history
  UNION ALL
  SELECT 'fact_prestige_history',
    COUNT(*), COUNT(login_id), COUNT(*) - COUNT(login_id)
  FROM fact_prestige_history
  UNION ALL
  SELECT 'fact_persistence_history',
    COUNT(*), COUNT(login_id), COUNT(*) - COUNT(login_id)
  FROM fact_persistence_history
")
print(verification)

constraints <- dbGetQuery(pool, "
  SELECT 
    tc.table_name,
    tc.constraint_name,
    string_agg(kcu.column_name, ', ' ORDER BY kcu.ordinal_position) as columns
  FROM information_schema.table_constraints tc
  JOIN information_schema.key_column_usage kcu
    ON tc.constraint_name = kcu.constraint_name
    AND tc.table_schema = kcu.table_schema
  WHERE tc.table_name IN (
    'fact_airr_history',
    'fact_presence_history',
    'fact_perception_history',
    'fact_prestige_history',
    'fact_persistence_history'
  )
  AND tc.constraint_type = 'PRIMARY KEY'
  GROUP BY tc.table_name, tc.constraint_name
  ORDER BY tc.table_name
")
print(constraints)

industry_check <- dbGetQuery(pool, "
  SELECT 
    COUNT(*) as total_tracking_rows,
    COUNT(industry) as rows_with_industry,
    COUNT(*) - COUNT(industry) as rows_still_null
  FROM fact_user_brands_tracked
")
print(industry_check)


dbGetQuery(pool, "
  SELECT column_name, data_type 
  FROM information_schema.columns 
  WHERE table_name = 'fact_profile_brand_history'
  ORDER BY ordinal_position
")

dbGetQuery(pool, "
  SELECT column_name, data_type 
  FROM information_schema.columns 
  WHERE table_name = 'dim_customer_profile'
  ORDER BY ordinal_position
")


dbGetQuery(pool, "
  SELECT column_name, data_type 
  FROM information_schema.columns 
  WHERE table_name = 'fact_profile_query_history'
  ORDER BY ordinal_position
")


# Check if PK exists
dbGetQuery(pool, "
  SELECT tc.constraint_name, 
         string_agg(kcu.column_name, ', ' ORDER BY kcu.ordinal_position) as columns
  FROM information_schema.table_constraints tc
  JOIN information_schema.key_column_usage kcu
    ON tc.constraint_name = kcu.constraint_name
  WHERE tc.table_name = 'fact_profile_query_history'
    AND tc.constraint_type = 'PRIMARY KEY'
  GROUP BY tc.constraint_name
")

# If no PK exists, add it
dbExecute(pool, "
  ALTER TABLE fact_profile_query_history
  ADD CONSTRAINT IF NOT EXISTS fact_profile_query_history_pkey
  PRIMARY KEY (profile_id, brand_id, query_id, date)
")

# Same for fact_profile_brand_history
dbGetQuery(pool, "
  SELECT tc.constraint_name,
         string_agg(kcu.column_name, ', ' ORDER BY kcu.ordinal_position) as columns
  FROM information_schema.table_constraints tc
  JOIN information_schema.key_column_usage kcu
    ON tc.constraint_name = kcu.constraint_name
  WHERE tc.table_name = 'fact_profile_brand_history'
    AND tc.constraint_type = 'PRIMARY KEY'
  GROUP BY tc.constraint_name
")

dbExecute(pool, "
  ALTER TABLE fact_profile_brand_history
  ADD CONSTRAINT IF NOT EXISTS fact_profile_brand_history_pkey
  PRIMARY KEY (profile_id, brand_id, date)
")

ttt <- dbGetQuery(pool, "SELECT pid, now() - query_start AS duration, state, query
FROM pg_stat_activity
WHERE state != 'idle'
ORDER BY duration DESC;")



# ============================================
# Manual walkthrough — post-onboarding scoring
# Run this line by line in an R console
# ============================================

# Load everything the app loads
source("global.R")

# Create a direct connection (not pool)
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = Sys.getenv("DB_PORT"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# ============================================
# Step 1: Look up the user and their data
# ============================================

login_id <- dbGetQuery(con,
                       "SELECT login_id FROM dim_user WHERE email = 'apple@apple.com'"
)$login_id[1]
cat("login_id:", login_id, "\n")

# Get main brand
main_brand <- dbGetQuery(con, "
  SELECT b.brand_id, b.brand_name
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE ubt.login_id = $1 AND ubt.main_brand_flag = TRUE
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)",
                         params = list(login_id))
cat("Main brand:", main_brand$brand_name, "(ID:", main_brand$brand_id, ")\n")

brand_name_copy <- main_brand$brand_name[1]
brand_id_copy   <- main_brand$brand_id[1]
login_id_copy   <- login_id

# Get competitors
competitors <- dbGetQuery(con, "
  SELECT b.brand_id, b.brand_name
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE ubt.login_id = $1 AND ubt.main_brand_flag = FALSE
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)",
                          params = list(login_id))
cat("Competitors:", nrow(competitors), "\n")
print(competitors)

comp_list     <- competitors$brand_name
comp_ids_copy <- setNames(as.list(competitors$brand_id), competitors$brand_name)

# Get tracked prompts
prompts_tracked <- dbGetQuery(con, "
  SELECT dq.query_id, dq.query_string
  FROM fact_user_queries_tracked uqt
  JOIN dim_query dq ON dq.query_id = uqt.query_id
  WHERE uqt.login_id = $1
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)",
                              params = list(login_id))
cat("Prompts:", nrow(prompts_tracked), "\n")
print(prompts_tracked)

prompt_ids_copy <- setNames(
  as.list(prompts_tracked$query_id),
  prompts_tracked$query_string
)
prompts_copy <- prompts_tracked$query_string

# ============================================
# Step 2: Score main brand (this is the big one)
# ============================================

cat("\n=== SCORING MAIN BRAND ===\n")
cat("Brand:", brand_name_copy, "\n")
cat("Time:", format(Sys.time(), "%H:%M:%S"), "\n\n")

t0 <- Sys.time()
tryCatch({
  user_create_airr(con, brand_name_copy, login_id_copy)
  cat("\n✓ Main brand scored in",
      round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "seconds\n")
}, error = function(e) {
  cat("\n✗ FAILED:", e$message, "\n")
})

# ============================================
# Step 3: Score main brand prompts
# ============================================

cat("\n=== SCORING MAIN BRAND PROMPTS ===\n")

if (length(prompts_copy) > 0) {
  for (pt in names(prompt_ids_copy)) {
    qid <- prompt_ids_copy[[pt]]
    cat("\n  Prompt:", substr(pt, 1, 60), "(ID:", qid, ")\n")
    cat("  Time:", format(Sys.time(), "%H:%M:%S"), "\n")
    
    t1 <- Sys.time()
    tryCatch({
      create_prompt_airr(con, brand_id_copy, pt, qid, login_id_copy)
      cat("  ✓ Done in", round(as.numeric(difftime(Sys.time(), t1, units = "secs")), 1), "s\n")
    }, error = function(e) {
      cat("  ✗ FAILED:", e$message, "\n")
    })
  }
} else {
  cat("  No prompts to score\n")
}

# ============================================
# Step 4: Score each competitor
# ============================================

cat("\n=== SCORING COMPETITORS ===\n")

for (comp_name in comp_list) {
  comp_id <- comp_ids_copy[[comp_name]]
  
  cat("\n--- Competitor:", comp_name, "(ID:", comp_id, ") ---\n")
  cat("Time:", format(Sys.time(), "%H:%M:%S"), "\n")
  
  # Check if already has scores
  has_scores <- dbGetQuery(con,
                           "SELECT COUNT(*) as cnt FROM fact_airr_history
     WHERE brand_id = $1 AND login_id = $2",
                           params = list(comp_id, login_id_copy))$cnt > 0
  cat("Already has scores:", has_scores, "\n")
  
  if (!has_scores) {
    t2 <- Sys.time()
    tryCatch({
      user_create_airr(con, comp_name, login_id_copy)
      cat("✓ Brand scored in",
          round(as.numeric(difftime(Sys.time(), t2, units = "secs")), 1), "s\n")
    }, error = function(e) {
      cat("✗ Brand scoring FAILED:", e$message, "\n")
    })
  } else {
    cat("Skipping brand scoring (already exists)\n")
  }
  
  # Score prompts for this competitor
  if (length(prompts_copy) > 0) {
    for (pt in names(prompt_ids_copy)) {
      qid <- prompt_ids_copy[[pt]]
      cat("  Prompt:", substr(pt, 1, 50), "...")
      
      t3 <- Sys.time()
      tryCatch({
        create_prompt_airr(con, comp_id, pt, qid, login_id_copy)
        cat(" ✓", round(as.numeric(difftime(Sys.time(), t3, units = "secs")), 1), "s\n")
      }, error = function(e) {
        cat(" ✗ FAILED:", e$message, "\n")
      })
    }
  }
}

# ============================================
# Step 5: Verify results
# ============================================

cat("\n\n=== VERIFICATION ===\n")

airr_results <- dbGetQuery(con, "
  SELECT b.brand_name, fa.airr_score, fa.date
  FROM fact_airr_history fa
  JOIN dim_brand b ON b.brand_id = fa.brand_id
  WHERE fa.login_id = $1
  ORDER BY fa.airr_score DESC",
                           params = list(login_id))
cat("\nAIRR scores:\n")
print(airr_results)

query_results <- dbGetQuery(con, "
  SELECT b.brand_name, dq.query_string, fqh.airr_score
  FROM fact_query_history fqh
  JOIN dim_brand b ON b.brand_id = fqh.brand_id
  JOIN dim_query dq ON dq.query_id = fqh.query_id
  WHERE fqh.brand_id IN (
    SELECT brand_id FROM fact_user_brands_tracked
    WHERE login_id = $1
      AND date_valid_from <= CURRENT_DATE
      AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
  )
  ORDER BY dq.query_string, fqh.airr_score DESC",
                            params = list(login_id))
cat("\nQuery scores:\n")
print(query_results)

# Clean up
dbDisconnect(con)
cat("\n=== DONE ===\n")
cat("Total time:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "minutes\n")






# migration --------------
dbExecute(pool, "
  CREATE TABLE dim_job_queue (
  job_id          SERIAL PRIMARY KEY,
  job_type        VARCHAR(50) NOT NULL,
  login_id        INTEGER NOT NULL,
  payload         JSONB NOT NULL DEFAULT '{}',
  status          VARCHAR(20) NOT NULL DEFAULT 'pending',
  created_at      TIMESTAMP NOT NULL DEFAULT NOW(),
  started_at      TIMESTAMP,
  completed_at    TIMESTAMP,
  error_message   TEXT,
  
  CONSTRAINT chk_status CHECK (status IN ('pending', 'running', 'complete', 'failed'))
);
")

dbExecute(pool, "CREATE INDEX idx_job_queue_status ON dim_job_queue (status) WHERE status = 'pending';")
dbExecute(pool, "CREATE INDEX idx_job_queue_login ON dim_job_queue (login_id);")

ttt <- dbGetQuery(pool, "SELECT pg_size_pretty(pg_database_size(current_database()));")

ttt <- dbGetQuery(pool, "SELECT * FROM dim_job_queue;")





library(DBI)
library(RPostgres)
library(jsonlite)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = Sys.getenv("DB_PORT"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Find an existing user/brand to test with
test <- dbGetQuery(con, "
  SELECT ubt.login_id, b.brand_id, b.brand_name
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE ubt.main_brand_flag = TRUE
  LIMIT 1")

cat("Inserting test job for:", test$brand_name[1], "/ user", test$login_id[1], "\n")

dbExecute(con, "
  INSERT INTO dim_job_queue (job_type, login_id, payload)
  VALUES ('score_brand', $1, $2)",
          params = list(
            test$login_id[1],
            toJSON(list(
              brand_name = test$brand_name[1],
              brand_id   = test$brand_id[1]
            ), auto_unbox = TRUE)
          ))

cat("Job inserted! Check dim_job_queue:\n")
print(dbGetQuery(con, "SELECT * FROM dim_job_queue ORDER BY created_at DESC LIMIT 5"))

dbDisconnect(con)

con <- make_con()
dbGetQuery(con, "
  SELECT job_id, job_type, payload 
  FROM dim_job_queue 
  WHERE job_type = 'score_brand_prompts' 
  ORDER BY created_at DESC 
  LIMIT 1")



# ============================================
# worker.R — Job queue processor
# Runs continuously on the worker server
# ============================================

library(DBI)
library(RPostgres)
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(glue)
library(lubridate)

setwd("~/airr")

source("global_scripts/app_helper_functions.R")
source("global_scripts/chatgpt_functions.R")
source("global_scripts/queries_send_return.R")
source("global_scripts/presence.R")
source("global_scripts/perception.R")
source("global_scripts/prestige.R")
source("global_scripts/persistence.R")
source("global_scripts/full_airr_score.R")
source("global_scripts/upload_functions.R")
source("global_scripts/profile_scoring.R")

message("=== AiRR Worker Started ===")
message(sprintf("Time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

poll_interval <- 5  # seconds between checks when idle

while (TRUE) {
  
  con <- NULL
  
  tryCatch({
    con <- make_con()
    
    # Claim the next pending job (atomic — prevents two workers grabbing the same job)
    job <- dbGetQuery(con, "
      UPDATE dim_job_queue
      SET status = 'running', started_at = NOW()
      WHERE job_id = (
        SELECT job_id FROM dim_job_queue
        WHERE status = 'pending'
        ORDER BY created_at
        LIMIT 1
        FOR UPDATE SKIP LOCKED
      )
      RETURNING *")
    
    if (nrow(job) == 0) {
      dbDisconnect(con)
      Sys.sleep(poll_interval)
      next
    }
    
    job_id   <- job$job_id[1]
    job_type <- job$job_type[1]
    lid      <- job$login_id[1]
    payload  <- fromJSON(job$payload[1])
    
    message(sprintf("\n=== JOB %d: %s (user %d) [%s] ===",
                    job_id, job_type, lid,
                    format(Sys.time(), "%H:%M:%S")))
    
    t0 <- Sys.time()
    
    switch(job_type,
           
           # ---- Score a single brand (main or competitor) ----
           "score_brand" = ,
           "score_competitor" = {
             brand_name <- payload$brand_name
             if (is.null(brand_name) || !nzchar(brand_name)) {
               brand_name <- dbGetQuery(con,
                                        "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                                        params = list(payload$brand_id))$brand_name[1]
             }
             
             message(sprintf("  Scoring brand: %s (ID: %s)", brand_name, payload$brand_id))
             user_create_airr(con, brand_name, lid)
             
             if (!is.null(payload$prompts) && length(payload$prompts) > 0) {
               prompt_names <- names(payload$prompts)
               for (pt in prompt_names) {
                 qid <- payload$prompts[[pt]]
                 message(sprintf("    Prompt: %s (ID: %s)", substr(pt, 1, 50), qid))
                 tryCatch(
                   create_prompt_airr(con, payload$brand_id, pt, qid, lid),
                   error = function(e) message(sprintf("    ✗ Prompt failed: %s", e$message))
                 )
               }
             }
           },
           
           # ---- Score a brand + its prompts ----
           "score_brand_prompts" = {
             # Look up brand_name if not in payload
             brand_name <- payload$brand_name
             if (is.null(brand_name) || !nzchar(brand_name)) {
               brand_name <- dbGetQuery(con,
                                        "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                                        params = list(payload$brand_id))$brand_name[1]
             }
             
             message(sprintf("  Scoring brand + prompts: %s (ID: %s)",
                             brand_name, payload$brand_id))
             user_create_airr(con, brand_name, lid)
             
             if (!is.null(payload$prompts) && length(payload$prompts) > 0) {
               prompt_names <- names(payload$prompts)
               for (pt in prompt_names) {
                 qid <- payload$prompts[[pt]]
                 message(sprintf("    Prompt: %s (ID: %s)", substr(pt, 1, 50), qid))
                 tryCatch(
                   create_prompt_airr(con, payload$brand_id, pt, qid, lid),
                   error = function(e) message(sprintf("    ✗ Prompt failed: %s", e$message))
                 )
               }
             }
           },
           
           # ---- Score all brands for a single query ----
           "score_query" = {
             message(sprintf("  Scoring query: %s (ID: %s) for %d brands",
                             substr(payload$query_string, 1, 50),
                             payload$query_id,
                             length(payload$brand_ids)))
             create_prompt_airr_multiple(con, payload$brand_ids,
                                         payload$query_string,
                                         payload$query_id, lid)
           },
           
           # ---- Score a customer profile ----
           "score_profile" = {
             message(sprintf("  Scoring profile: %s", payload$profile_id))
             score_profile(con, lid, payload$profile_id)
           },
           
           # ---- Unknown job type ----
           {
             stop(sprintf("Unknown job type: %s", job_type))
           }
    )
    
    elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    
    dbExecute(con, "
      UPDATE dim_job_queue
      SET status = 'complete', completed_at = NOW()
      WHERE job_id = $1",
              params = list(job_id))
    
    message(sprintf("=== JOB %d COMPLETE: %ss ===", job_id, elapsed))
    
  }, error = function(e) {
    message(sprintf("=== JOB ERROR: %s ===", e$message))
    
    # Try to mark the job as failed
    tryCatch({
      if (!is.null(con) && dbIsValid(con)) {
        # Get job_id if we have it
        if (exists("job_id") && !is.null(job_id)) {
          dbExecute(con, "
            UPDATE dim_job_queue
            SET status = 'failed', completed_at = NOW(), error_message = $1
            WHERE job_id = $2",
                    params = list(e$message, job_id))
        }
      }
    }, error = function(e2) {
      message(sprintf("  Could not update job status: %s", e2$message))
    })
  })
  
  # Always disconnect
  tryCatch({
    if (!is.null(con) && dbIsValid(con)) dbDisconnect(con)
  }, error = function(e) invisible(NULL))
}


# ---------------------------

# run_once_set_all_enterprise.R
# Run this ONCE on the web server or any machine with DB access
# Rscript run_once_set_all_enterprise.R

library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = Sys.getenv("DB_PORT"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Get Enterprise tier ID
ent <- dbGetQuery(con,
                  "SELECT subscription_level_id 
   FROM dim_subscription 
   WHERE subscription_name = 'Enterprise'
   LIMIT 1")

if (nrow(ent) == 0) stop("Enterprise tier not found in dim_subscription")

ent_id <- ent$subscription_level_id[1]
cat("Enterprise subscription_level_id:", ent_id, "\n")

# Get all users
all_users <- dbGetQuery(con, "SELECT login_id FROM dim_user")
cat("Total users:", nrow(all_users), "\n")

# Close off any non-Enterprise active subs
closed <- dbExecute(con,
                    "UPDATE fact_user_sub_level
   SET date_valid_to = CURRENT_DATE - 1
   WHERE subscription_level_id != $1
     AND date_valid_to >= CURRENT_DATE",
                    params = list(ent_id))
cat("Closed non-Enterprise subscriptions:", closed, "\n")

# Insert Enterprise for all users who don't already have it active
inserted <- 0
for (lid in all_users$login_id) {
  existing <- dbGetQuery(con,
                         "SELECT 1 FROM fact_user_sub_level
     WHERE login_id = $1
       AND subscription_level_id = $2
       AND date_valid_from <= CURRENT_DATE
       AND date_valid_to >= CURRENT_DATE",
                         params = list(lid, ent_id))
  
  if (nrow(existing) == 0) {
    dbExecute(con,
              "INSERT INTO fact_user_sub_level
         (login_id, subscription_level_id, date_valid_from, date_valid_to)
       VALUES ($1, $2, CURRENT_DATE, '2099-12-31')",
              params = list(lid, ent_id))
    inserted <- inserted + 1
    cat("  ✓ Enterprise set for login_id:", lid, "\n")
  } else {
    cat("  — Already Enterprise: login_id:", lid, "\n")
  }
}

cat("\nDone. Inserted:", inserted, "new Enterprise subscriptions\n")
dbDisconnect(con)


sudo R --vanilla -e "
install.packages(
  c(
    'reticulate'
  ),
  repos = 'https://cloud.r-project.org',
  lib = '/usr/local/lib/R/site-library'
)
"
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = "172.31.39.152",
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  # Connection-level statement timeout — kills any query running > 30s
  options  = "-c statement_timeout=30000"
)



con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv('DB_NAME'),
  host     = Sys.getenv('DB_HOST'),
  port     = Sys.getenv('DB_PORT'),
  user     = Sys.getenv('DB_USER'),
  password = Sys.getenv('DB_PASSWORD')
)

# First — show all users and their brands so we can confirm which login_id is Blue Owl
cat('=== All users and brands ===\n')
users <- dbGetQuery(con, '
  SELECT du.login_id, du.email, db.brand_name, ubt.main_brand_flag, ubt.date_valid_from
  FROM dim_user du
  JOIN fact_user_brands_tracked ubt ON ubt.login_id = du.login_id
  JOIN dim_brand db ON db.brand_id = ubt.brand_id
  ORDER BY du.login_id, ubt.main_brand_flag DESC
')
print(users)

# Show what scores exist for Under Armour broken down by user
cat('\n=== Under Armour scores by user ===\n')
ua_scores <- dbGetQuery(con, "
  SELECT 
    du.email,
    du.login_id,
    fah.date,
    fah.airr_score
  FROM fact_airr_history fah
  JOIN dim_brand db  ON db.brand_id  = fah.brand_id
  JOIN dim_user  du  ON du.login_id  = fah.login_id
  WHERE lower(db.brand_name) = 'under armour'
  ORDER BY du.login_id, fah.date
")
print(ua_scores)

dbDisconnect(con)

preview <- dbGetQuery(con, "
  SELECT * FROM fact_user_brands_tracked
  WHERE login_id = 10
    AND brand_id > 100
")

rows_deleted <- dbExecute(con, "
  DELETE FROM fact_user_brands_tracked
  WHERE login_id = 10
    AND brand_id > 100
")
cat('Rows deleted:', rows_deleted, '\n')

ttt <- dbGetQuery(con, "
  SELECT 
  u.login_id, 
  u.email,
  ds.subscription_name,
  fus.date_valid_from,
  fus.date_valid_to
FROM dim_user u
JOIN fact_user_sub_level fus ON fus.login_id = u.login_id
JOIN dim_subscription ds ON ds.subscription_level_id = fus.subscription_level_id
WHERE u.email = 'fmt@fmt.com';
")

pers34 <- dbGetQuery(con, "SELECT 
  fpbh.profile_id,
  dcp.profile_name,
  fpbh.brand_id,
  db.brand_name,
  fpbh.date,
  fpbh.airr_score,
  fpbh.presence_score
FROM fact_profile_brand_history fpbh
JOIN dim_customer_profile dcp ON dcp.profile_id = fpbh.profile_id
JOIN dim_brand db ON db.brand_id = fpbh.brand_id
WHERE fpbh.profile_id IN (
  SELECT profile_id FROM fact_user_profiles_tracked WHERE login_id = 35
)
ORDER BY fpbh.date DESC
LIMIT 20;"
)

jqjq <- dbGetQuery(con, "SELECT * from dim_job_queue where login_id = 34"
)

dbExecute(con, "INSERT INTO dim_job_queue (job_type, login_id, payload)
VALUES 
  ('score_profile', 34, '{\"profile_id\": 3}'),
  ('score_profile', 34, '{\"profile_id\": 4}')")




source("global.R")

con <- make_con()

# Test score_profile directly with full error visibility — no tryCatch
profile_id <- 3
login_id   <- 34

# Step 1: does the profile exist?
profile <- dbGetQuery(con,
                      "SELECT profile_name, profile_descriptor 
   FROM dim_customer_profile WHERE profile_id = $1",
                      params = list(profile_id))
print(profile)

# Step 2: does the user have brands?
brands <- dbGetQuery(con, "
  SELECT b.brand_id, b.brand_name, b.brand_reach,
         b.reach_country, b.reach_region, b.reach_postcode,
         ubt.main_brand_flag
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE ubt.login_id = $1
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
", params = list(login_id))
print(brands)

# Step 3: try scoring just the first brand with no tryCatch
if (nrow(brands) > 0) {
  bid        <- brands$brand_id[1]
  brand_name <- brands$brand_name[1]
  reach      <- brands$brand_reach[1] %||% "global"
  country    <- brands$reach_country[1]
  region     <- brands$reach_region[1]
  postcode   <- brands$reach_postcode[1]
  
  if (is.na(country))  country  <- NULL
  if (is.na(region))   region   <- NULL
  if (is.na(postcode)) postcode <- NULL
  
  descriptor <- profile$profile_descriptor[1]
  prefix     <- paste0(descriptor, " ")
  
  message(sprintf("Testing brand: %s (ID: %d)", brand_name, bid))
  
  # Call score_profile_brand directly — errors will surface immediately
  score_profile_brand(
    con            = con,
    brand_id       = bid,
    brand_name     = brand_name,
    profile_id     = profile_id,
    login_id       = login_id,
    prefix         = prefix,
    brand_reach    = reach,
    reach_country  = country,
    reach_region   = region,
    reach_postcode = postcode,
    model          = "gpt-4o-mini"
  )
}

dbDisconnect(con)



dbGetQuery(con, "
  SELECT 
    fpbh.profile_id,
    dcp.profile_name,
    fpbh.brand_id,
    db.brand_name,
    fpbh.date,
    fpbh.airr_score,
    fpbh.presence_score,
    fpbh.perception_score,
    fpbh.prestige_score,
    fpbh.persistence_score
  FROM fact_profile_brand_history fpbh
  JOIN dim_customer_profile dcp ON dcp.profile_id = fpbh.profile_id
  JOIN dim_brand db ON db.brand_id = fpbh.brand_id
  WHERE fpbh.profile_id IN (
    SELECT profile_id FROM fact_user_profiles_tracked WHERE login_id = 34
  )
  ORDER BY fpbh.date DESC
")

dbGetQuery(con, "
  SELECT 
    upt.login_id,
    upt.profile_id,
    dcp.profile_name,
    upt.date_valid_from,
    upt.date_valid_to
  FROM fact_user_profiles_tracked upt
  JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
  WHERE upt.login_id = 35
  ORDER BY upt.date_valid_from DESC
")


dbGetQuery(con, "
  SELECT COUNT(*) as row_count
  FROM fact_profile_brand_history fpbh
  WHERE fpbh.profile_id IN (
    SELECT profile_id FROM fact_user_profiles_tracked WHERE login_id = 35
  )
")
dbGetQuery(con, "
  SELECT job_id, job_type, status, error_message, payload, created_at, completed_at
  FROM dim_job_queue
  WHERE login_id = 35
    AND job_type = 'score_profile'
  ORDER BY created_at DESC
")

dbExecute(pool, "
  INSERT INTO dim_job_queue (job_type, login_id, payload)
  VALUES ($1, $2, $3)",
          params = list(
            'score_profile',
            35,
            toJSON(list(profile_id = 16), auto_unbox = TRUE)
          ))

dbGetQuery(pool, "
  SELECT job_id, job_type, status, created_at 
  FROM dim_job_queue 
  WHERE login_id = 35 
  ORDER BY created_at DESC 
  LIMIT 5
")

# Check for very recent activity in the job queue (last 30 mins)
dbGetQuery(pool, "
  SELECT jq.login_id, u.email, jq.job_type, jq.status, jq.created_at
  FROM dim_job_queue jq
  JOIN dim_user u ON u.login_id = jq.login_id
  WHERE jq.created_at > NOW() - INTERVAL '30 minutes'
  ORDER BY jq.created_at DESC
")




dbGetQuery(pool, "
  SELECT count(*) as total_connections, 
         state,
         wait_event_type,
         wait_event
  FROM pg_stat_activity 
  WHERE datname = current_database()
  GROUP BY state, wait_event_type, wait_event
  ORDER BY total_connections DESC
")




# Requeue the failed job
dbExecute(pool, "
  INSERT INTO dim_job_queue (job_type, login_id, payload)
  VALUES ($1, $2, $3)",
          params = list(
            'score_competitor',
            37,
            toJSON(list(
              brand_name = "Alaska Airlines",
              brand_id   = 148,
              prompts    = list(
                "What are the best airlines for long-haul flights from New York?" = 223,
                "What are the top-rated airlines for customer service in the U.S.?" = 226,
                "Which airline offers the cheapest flights from Los Angeles to Chicago?" = 224,
                "Which airline has the best rewards program for frequent flyers in the U.S.?" = 227,
                "Can you recommend a family-friendly airline for travel within the United States?" = 225,
                "What are the best airlines for International flights to Europe this summer from any major East coast airport?" = 228
              )
            ), auto_unbox = TRUE)
          ))


dbGetQuery(pool, "
  SELECT b.brand_id, b.brand_name, ubt.main_brand_flag,
         b.brand_reach, b.reach_country
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE ubt.login_id = 37
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
")

dbGetQuery(pool, "
  SELECT b.brand_name, fa.date, fa.airr_score,
         fpres.overall_score as presence,
         fperc.perception_score as perception,
         fprest.prestige_score as prestige,
         fpers.persistence_score as persistence
  FROM fact_airr_history fa
  JOIN dim_brand b ON b.brand_id = fa.brand_id
  LEFT JOIN fact_presence_history fpres 
    ON fpres.brand_id = fa.brand_id AND fpres.date = fa.date AND fpres.login_id = fa.login_id
  LEFT JOIN fact_perception_history fperc 
    ON fperc.brand_id = fa.brand_id AND fperc.date = fa.date AND fperc.login_id = fa.login_id
  LEFT JOIN fact_prestige_history fprest 
    ON fprest.brand_id = fa.brand_id AND fprest.date = fa.date AND fprest.login_id = fa.login_id
  LEFT JOIN fact_persistence_history fpers 
    ON fpers.brand_id = fa.brand_id AND fpers.date = fa.date AND fpers.login_id = fa.login_id
  WHERE fa.login_id = 37
  ORDER BY fa.date DESC, b.brand_name
")

brand_name       date airr_score presence perception prestige persistence
1  American Airlines 2026-03-09    74.3945    96.05     70.008  40.6783       96.05
2              Delta 2026-03-09    76.9300    88.62     69.712  64.5494       88.62
3    JetBlue Airways 2026-03-09    59.7608    48.05     77.094  60.0405       48.05
4 Southwest Airlines 2026-03-09    66.5028    61.28     74.074  66.8184       61.28
5    United Airlines 2026-03-09    63.3815    70.47     70.708  41.8305       70.47
6             Virgin 2026-03-09    29.5929     0.00     74.384  29.1109        0.00

dbGetQuery(pool, "
  SELECT b.brand_name, fph.date, 
         fph.prestige_score,
         fph.prestige_rank_comps_brands
  FROM fact_prestige_history fph
  JOIN dim_brand b ON b.brand_id = fph.brand_id
  WHERE fph.login_id = 37
  ORDER BY fph.date DESC, b.brand_name
")

dbExecute(pool, "
  INSERT INTO dim_job_queue (job_type, login_id, payload)
  VALUES ($1, $2, $3)",
          params = list(
            'score_competitor',
            37,
            toJSON(list(
              brand_name = "Alaska Airlines",
              brand_id   = 148L,
              prompts    = list(
                "What are the best airlines for long-haul flights from New York?" = 223L,
                "What are the top-rated airlines for customer service in the U.S.?" = 226L,
                "Which airline offers the cheapest flights from Los Angeles to Chicago?" = 224L,
                "Which airline has the best rewards program for frequent flyers in the U.S.?" = 227L,
                "Can you recommend a family-friendly airline for travel within the United States?" = 225L,
                "What are the best airlines for International flights to Europe this summer from any major East coast airport?" = 228L
              )
            ), auto_unbox = TRUE)
          ))


# Simulate dash_latest_scores for user 37
brands <- dbGetQuery(pool, "
  SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
  FROM fact_user_brands_tracked ubt
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE ubt.login_id = 37
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
")

brand_ids    <- brands$brand_id
login_id     <- 37
n            <- length(brand_ids)
placeholders <- paste0("$", 2:(n + 1), collapse = ", ")

query <- sprintf("
  WITH latest_dates AS (
    SELECT brand_id, MAX(date) as latest_date
    FROM fact_airr_history
    WHERE login_id = $1
      AND brand_id IN (%s)
      AND airr_score IS NOT NULL
    GROUP BY brand_id
  )
  SELECT 
    db.brand_name, db.brand_id,
    fa.airr_score,
    fpres.overall_score  as presence_score,
    fperc.perception_score,
    fprest.prestige_score,
    fpers.persistence_score,
    fa.date
  FROM dim_brand db
  INNER JOIN latest_dates ld ON db.brand_id = ld.brand_id
  INNER JOIN fact_airr_history fa
    ON db.brand_id = fa.brand_id AND fa.date = ld.latest_date
    AND fa.login_id = $1
  LEFT JOIN fact_presence_history fpres
    ON db.brand_id = fpres.brand_id AND fpres.date = ld.latest_date
    AND fpres.login_id = $1
  LEFT JOIN fact_perception_history fperc
    ON db.brand_id = fperc.brand_id AND fperc.date = ld.latest_date
    AND fperc.login_id = $1
  LEFT JOIN fact_prestige_history fprest
    ON db.brand_id = fprest.brand_id AND fprest.date = ld.latest_date
    AND fprest.login_id = $1
  LEFT JOIN fact_persistence_history fpers
    ON db.brand_id = fpers.brand_id AND fpers.date = ld.latest_date
    AND fpers.login_id = $1
  ORDER BY fa.airr_score DESC
", placeholders)

result <- dbGetQuery(pool, query, params = as.list(c(login_id, brand_ids)))
print(result)
# Alaska Airlines should be missing from this since it has no scores
# That's fine — the WITH latest_dates will just exclude it
# So the dashboard query itself isn't the crash...


brand_name <- "Alaska Airlines"
brand_id   <- 148L
login_id   <- 37L

cat("=== Step 1: prestige history ===\n")
prestige_hist <- dbGetQuery(con, "
  SELECT brand_id, login_id, date, prestige_rank_comps_brands
  FROM fact_prestige_history
  WHERE brand_id = $1 AND login_id = $2
  ORDER BY date DESC
", params = list(brand_id, login_id))
print(prestige_hist)
cat("Rows:", nrow(prestige_hist), "\n")


dbExecute(pool, "
  INSERT INTO dim_job_queue (job_type, login_id, payload)
  VALUES ($1, $2, $3)",
          params = list(
            'score_competitor',
            37,
            toJSON(list(
              brand_name = "Alaska Airlines",
              brand_id   = 148L,
              prompts    = list(
                "What are the best airlines for long-haul flights from New York?" = 223L,
                "What are the top-rated airlines for customer service in the U.S.?" = 226L,
                "Which airline offers the cheapest flights from Los Angeles to Chicago?" = 224L,
                "Which airline has the best rewards program for frequent flyers in the U.S.?" = 227L,
                "Can you recommend a family-friendly airline for travel within the United States?" = 225L,
                "What are the best airlines for International flights to Europe this summer from any major East coast airport?" = 228L
              )
            ), auto_unbox = TRUE)
          ))


jqjq <- dbGetQuery(pool,'select * from dim_job_queue')


dbGetQuery(pool, "
  SELECT dq.query_id, dq.query_string
  FROM fact_user_queries_tracked uqt
  JOIN dim_query dq ON dq.query_id = uqt.query_id
  WHERE uqt.login_id = 37
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
")

dbGetQuery(pool, "
  SELECT brand_id, brand_name, brand_reach, reach_country
  FROM dim_brand
  WHERE brand_id = 144
")

con <- make_con()

rel_responses <- prompt_queries(
  rep("What are the best airlines for long-haul flights from New York?", 10),
  model = "gpt-4o-mini"
)

dbGetQuery(pool, "
  SELECT fqh.brand_id, db.brand_name, dq.query_string,
         fqh.date, fqh.presence_score, fqh.perception_score,
         fqh.prestige_score, fqh.airr_score
  FROM fact_query_history fqh
  JOIN dim_brand db ON db.brand_id = fqh.brand_id
  JOIN dim_query dq ON dq.query_id = fqh.query_id
  WHERE fqh.brand_id = 144
  ORDER BY fqh.date DESC, dq.query_string
")

dbGetQuery(pool, "
  SELECT fqh.*
  FROM fact_query_history fqh
  WHERE fqh.brand_id = 144
")

dbGetQuery(pool, "
  SELECT dq.query_id, dq.query_string
  FROM fact_user_queries_tracked uqt
  JOIN dim_query dq ON dq.query_id = uqt.query_id
  WHERE uqt.login_id = 37
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
  ORDER BY dq.query_id
")

dbGetQuery(pool, "
  SELECT dbq.brand_id, db.brand_name, dbq.query_id, dq.query_string, dbq.date_added
  FROM dim_brand_query dbq
  JOIN dim_brand db ON db.brand_id = dbq.brand_id
  JOIN dim_query dq ON dq.query_id = dbq.query_id
  WHERE dbq.brand_id IN (144, 145, 146, 147, 148, 149, 150)
  ORDER BY dbq.brand_id, dbq.query_id
")

dbGetQuery(pool, "
  SELECT fqh.brand_id, db.brand_name, fqh.query_id, dq.query_string,
         fqh.date, fqh.presence_score, fqh.perception_score,
         fqh.prestige_score, fqh.airr_score
  FROM fact_query_history fqh
  JOIN dim_brand db ON db.brand_id = fqh.brand_id
  JOIN dim_query dq ON dq.query_id = fqh.query_id
  WHERE fqh.brand_id IN (144, 145, 146, 147, 148, 149, 150)
  ORDER BY fqh.brand_id, fqh.query_id
")

brand_meta <- dbGetQuery(pool, "
  SELECT brand_name, brand_reach, reach_country, reach_postcode
  FROM dim_brand WHERE brand_id = 144
")

reach       <- brand_meta$brand_reach[1]
nm_country  <- brand_meta$reach_country[1]
nm_postcode <- brand_meta$reach_postcode[1]
if (is.na(nm_country))  nm_country  <- NULL
if (is.na(nm_postcode)) nm_postcode <- NULL

user_queries <- dbGetQuery(pool, "
  SELECT dq.query_id, dq.query_string
  FROM fact_user_queries_tracked uqt
  JOIN dim_query dq ON dq.query_id = uqt.query_id
  WHERE uqt.login_id = 37
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
")

cat("=== Resolved prompts that get sent to LLM ===\n")
for (i in 1:nrow(user_queries)) {
  resolved <- inject_near_me_location(
    user_queries$query_string[i], reach, nm_country, nm_postcode
  )
  cat(sprintf("\nQuery %d original:  %s\n", user_queries$query_id[i], user_queries$query_string[i]))
  cat(sprintf("Query %d resolved:  %s\n", user_queries$query_id[i], resolved))
}


prompts_list <- list(
  "What are the best airlines for long-haul flights from New York?" = 223L,
  "What are the top-rated airlines for customer service in the U.S.?" = 226L,
  "Which airline offers the cheapest flights from Los Angeles to Chicago?" = 224L,
  "Which airline has the best rewards program for frequent flyers in the U.S.?" = 227L,
  "Can you recommend a family-friendly airline for travel within the United States?" = 225L,
  "What are the best airlines for International flights to Europe this summer from any major East coast airport?" = 228L
)

brands_37 <- list(
  list(brand_id = 144L, brand_name = "Delta"),
  list(brand_id = 145L, brand_name = "American Airlines"),
  list(brand_id = 146L, brand_name = "United Airlines"),
  list(brand_id = 147L, brand_name = "Southwest Airlines"),
  list(brand_id = 148L, brand_name = "Alaska Airlines"),
  list(brand_id = 149L, brand_name = "JetBlue Airways"),
  list(brand_id = 150L, brand_name = "Virgin")
)

for (brand in brands_37) {
  dbExecute(pool, "
    INSERT INTO dim_job_queue (job_type, login_id, payload)
    VALUES ($1, $2, $3)",
            params = list(
              'score_competitor',
              37L,
              toJSON(list(
                brand_name = brand$brand_name,
                brand_id   = brand$brand_id,
                prompts    = prompts_list
              ), auto_unbox = TRUE)
            ))
  message(sprintf("Queued prompt scoring for %s (brand_id %d)", 
                  brand$brand_name, brand$brand_id))
}







# Get the actual responses for one prompt and walk through scoring manually
brand_name <- "Delta"
brand_id   <- 144L
login_id   <- 37L

# Get brand meta
brand_meta <- dbGetQuery(pool, "
  SELECT brand_name, brand_reach, reach_country, reach_postcode, reach_region
  FROM dim_brand WHERE brand_id = $1
", params = list(brand_id))

reach    <- brand_meta$brand_reach[1]
country  <- brand_meta$reach_country[1]
region   <- brand_meta$reach_region[1]
postcode <- brand_meta$reach_postcode[1]
if (is.na(country))  country  <- NULL
if (is.na(region))   region   <- NULL
if (is.na(postcode)) postcode <- NULL

# Use one prompt
query_string <- "What are the best airlines for long-haul flights from New York?"
query_resolved <- inject_near_me_location(query_string, reach, country, region, postcode)
cat("Resolved prompt:", query_resolved, "\n")

# Get responses
rel_responses <- prompt_queries(rep(query_resolved, 10), model = "gpt-4o-mini")

cat("\n=== Raw responses ===\n")
for (i in 1:nrow(rel_responses)) {
  cat(sprintf("\n--- Response %d ---\n", i))
  cat(rel_responses$responses[i], "\n")
}

# Check what presence_prompt_calc actually does with these
cat("\n=== Testing presence_prompt_calc ===\n")
presence_score <- presence_prompt_calc(brand_name, rel_responses)
cat("presence_score:", presence_score, "\n")

# Check each response individually for mentions
cat("\n=== Mention check per response ===\n")
for (i in 1:nrow(rel_responses)) {
  text <- rel_responses$responses[i]
  
  # Direct string detection
  contains_delta <- grepl("delta", text, ignore.case = TRUE)
  
  # What find_all_mentions returns
  mentions <- find_all_mentions(text, brand_name)
  
  cat(sprintf("Response %d | grepl: %s | find_all_mentions: %d hits\n",
              i, contains_delta, length(mentions)))
}

# Now look inside presence_prompt_calc to see what it's doing
# Share the function so we can check it
cat("\n=== presence_prompt_calc source ===\n")
print(presence_prompt_calc)





# Simulate exactly what the worker does for the score_competitor job
payload <- list(
  brand_name = "Delta",
  brand_id   = 144L,
  prompts    = list(
    "What are the best airlines for long-haul flights from New York?" = 223L,
    "What are the top-rated airlines for customer service in the U.S.?" = 226L,
    "Which airline offers the cheapest flights from Los Angeles to Chicago?" = 224L,
    "Which airline has the best rewards program for frequent flyers in the U.S.?" = 227L,
    "Can you recommend a family-friendly airline for travel within the United States?" = 225L,
    "What are the best airlines for International flights to Europe this summer from any major East coast airport?" = 228L
  )
)

lid <- 37L

# This is exactly what worker.R does
brand_name <- payload$brand_name
cat("brand_name:", brand_name, "\n")

# First it calls user_create_airr
cat("\n=== Step 1: user_create_airr ===\n")
tryCatch({
  user_create_airr(con, brand_name, lid)
  cat("user_create_airr OK\n")
}, error = function(e) cat("ERROR:", e$message, "\n"))

# Then it loops through prompts
cat("\n=== Step 2: prompt loop ===\n")
prompt_names <- names(payload$prompts)
cat("Number of prompts:", length(prompt_names), "\n")

for (pt in prompt_names) {
  qid <- payload$prompts[[pt]]
  cat(sprintf("\n--- Prompt: %s (ID: %s) ---\n", substr(pt, 1, 50), qid))
  
  tryCatch({
    create_prompt_airr(con, payload$brand_id, pt, qid, lid)
    cat("OK\n")
    
    # Check it actually wrote something
    check <- dbGetQuery(con, "
      SELECT presence_score, perception_score, airr_score
      FROM fact_query_history
      WHERE brand_id = $1 AND query_id = $2 AND date = $3
    ", params = list(payload$brand_id, qid, Sys.Date()))
    
    if (nrow(check) > 0) {
      cat(sprintf("  Written: presence=%.1f perception=%.1f airr=%.1f\n",
                  check$presence_score, check$perception_score, check$airr_score))
    } else {
      cat("  WARNING: nothing written to fact_query_history!\n")
    }
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    cat("Traceback:\n")
    traceback()
  })
}



prompts_list <- list(
  "What are the best airlines for long-haul flights from New York?" = 223L,
  "What are the top-rated airlines for customer service in the U.S.?" = 226L,
  "Which airline offers the cheapest flights from Los Angeles to Chicago?" = 224L,
  "Which airline has the best rewards program for frequent flyers in the U.S.?" = 227L,
  "Can you recommend a family-friendly airline for travel within the United States?" = 225L,
  "What are the best airlines for International flights to Europe this summer from any major East coast airport?" = 228L
)

brands_37 <- list(
  list(brand_id = 144L, brand_name = "Delta"),
  list(brand_id = 145L, brand_name = "American Airlines"),
  list(brand_id = 146L, brand_name = "United Airlines"),
  list(brand_id = 147L, brand_name = "Southwest Airlines"),
  list(brand_id = 148L, brand_name = "Alaska Airlines"),
  list(brand_id = 149L, brand_name = "JetBlue Airways"),
  list(brand_id = 150L, brand_name = "Virgin")
)

for (brand in brands_37) {
  dbExecute(pool, "
    INSERT INTO dim_job_queue (job_type, login_id, payload)
    VALUES ($1, $2, $3)",
            params = list(
              'score_competitor',
              37L,
              toJSON(list(
                brand_name = brand$brand_name,
                brand_id   = brand$brand_id,
                prompts    = prompts_list
              ), auto_unbox = TRUE)
            ))
  message(sprintf("Queued: %s (brand_id %d)", brand$brand_name, brand$brand_id))
}

# Confirm they're queued
dbGetQuery(pool, "
  SELECT job_id, job_type, status, payload, created_at
  FROM dim_job_queue
  WHERE login_id = 37
    AND status = 'pending'
  ORDER BY created_at DESC
")



# See all users, their brands, last score date and subscription
dbGetQuery(pool, "
  SELECT 
    u.login_id,
    u.email,
    u.date_added,
    COUNT(DISTINCT ubt.brand_id) as brands_tracked,
    COUNT(DISTINCT uqt.query_id) as queries_tracked,
    MAX(fa.date) as last_score_date,
    ds.subscription_name
  FROM dim_user u
  LEFT JOIN fact_user_brands_tracked ubt 
    ON ubt.login_id = u.login_id
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
  LEFT JOIN fact_user_queries_tracked uqt
    ON uqt.login_id = u.login_id
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
  LEFT JOIN fact_airr_history fa ON fa.login_id = u.login_id
  LEFT JOIN fact_user_sub_level fus 
    ON fus.login_id = u.login_id
    AND fus.date_valid_from <= CURRENT_DATE
    AND (fus.date_valid_to IS NULL OR fus.date_valid_to > CURRENT_DATE)
  LEFT JOIN dim_subscription ds 
    ON ds.subscription_level_id = fus.subscription_level_id
  GROUP BY u.login_id, u.email, u.date_added, ds.subscription_name
  ORDER BY last_score_date DESC NULLS LAST
")


# Set these login_ids to inactive — replace with your actual list
inactive_login_ids <- c(1	,
                        6	,
                        13	,
                        14	,
                        17	,
                        19	,
                        20	,
                        21	,
                        22	,
                        23	,
                        24	,
                        25	,
                        26	,
                        27	,
                        31	,
                        32	,
                        33	,
                        34	)  # <-- replace with real IDs

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






dbGetQuery(pool, "
  SELECT uqt.login_id, u.email, dq.query_string
  FROM fact_user_queries_tracked uqt
  JOIN dim_user u ON u.login_id = uqt.login_id
  JOIN dim_query dq ON dq.query_id = uqt.query_id
  WHERE uqt.query_id = 233
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
")

dbGetQuery(pool, "
  SELECT DISTINCT b.brand_id, b.brand_name, ubt.login_id,
         uqt.query_id
  FROM fact_user_queries_tracked uqt
  JOIN fact_user_brands_tracked ubt ON uqt.login_id = ubt.login_id
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE uqt.query_id = 233
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
  ORDER BY ubt.login_id, b.brand_name
")

brands_for_query <- dbGetQuery(pool, "
  SELECT DISTINCT b.brand_id, b.brand_name,
         b.brand_reach, b.reach_country, b.reach_region, b.reach_postcode,
         ubt.login_id
  FROM fact_user_queries_tracked uqt
  JOIN fact_user_brands_tracked ubt ON uqt.login_id = ubt.login_id
  JOIN dim_brand b ON b.brand_id = ubt.brand_id
  JOIN dim_brand_query dbq 
    ON dbq.brand_id = b.brand_id 
    AND dbq.query_id = uqt.query_id    -- <-- only brands explicitly linked to this query
  WHERE uqt.query_id = $1
    AND uqt.date_valid_from <= CURRENT_DATE
    AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
  ORDER BY b.brand_id, ubt.login_id",
                               params = 233)



brands_for_query <- dbGetQuery(pool, "
        SELECT DISTINCT b.brand_id, b.brand_name,
               b.brand_reach, b.reach_country, b.reach_region, b.reach_postcode,
               ubt.login_id
        FROM fact_user_queries_tracked uqt
        JOIN fact_user_brands_tracked ubt ON uqt.login_id = ubt.login_id
        JOIN dim_brand b ON b.brand_id = ubt.brand_id
        WHERE uqt.query_id = $1
          AND uqt.date_valid_from <= CURRENT_DATE
          AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
          AND ubt.date_valid_from <= CURRENT_DATE
          AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
        ORDER BY b.brand_id, ubt.login_id",
                               params = 233)







library(httr2)
library(jsonlite)

api_key    <- "airr_UxbinoA8ZHNjjCk2B5QwB8v9RRniAcPM02FC5tRanbZmvYar"
base_url   <- "https://airr.airrscore.com/api"


# ── Brand scores ──────────────────────────────────────────────────────────

brand_resp <- request(paste0(base_url, "/v1/brand-scores")) |>
  req_headers("X-API-Key" = api_key) |>
  req_url_query(
    from = "2024-01-01",
    to   = Sys.Date()
  ) |>
  req_perform()

brand_data <- resp_body_json(brand_resp, simplifyVector = TRUE)

# Overall scores as a data frame
brand_overall <- as.data.frame(brand_data$overall_scores)

# Persona splits as a data frame
brand_personas <- as.data.frame(brand_data$persona_scores)

print(head(brand_overall))
print(head(brand_personas))


resp <- request("http://54.162.252.165:8765/v1/brand-scores") |>
  req_headers("X-API-Key" = api_key) |>
  req_url_query(
    from = as.character(Sys.Date() - 30),
    to   = as.character(Sys.Date())
  ) |>
  req_perform()

cat("Status:", resp_status(resp), "\n")
data <- resp_body_json(resp, simplifyVector = TRUE)
cat("Overall score rows:", nrow(as.data.frame(data$overall_scores)), "\n")
cat("Persona score rows:", nrow(as.data.frame(data$persona_scores)), "\n")
print(head(as.data.frame(data$overall_scores)))



resp2 <- request("http://airr.airrscore.com/api/v1/brand-scores") |>
  req_headers("X-API-Key" = api_key) |>
  req_url_query(
    from = as.character(Sys.Date() - 30),
    to   = as.character(Sys.Date())
  ) |>
  req_perform()

cat("Status:", resp_status(resp2), "\n")



resp_direct <- request("http://3.65.100.153/api/v1/brand-scores") |>
  req_headers("X-API-Key" = api_key) |>
  req_url_query(
    from = as.character(Sys.Date() - 30),
    to   = as.character(Sys.Date())
  ) |>
  req_perform()

cat("Status:", resp_status(resp_direct), "\n")


resp <- request("http://api.airrscore.com/api/v1/brand-scores") |>
  req_headers("X-API-Key" = api_key) |>
  req_url_query(from = as.character(Sys.Date() - 30),
                to   = as.character(Sys.Date())) |>
  req_perform()

cat("Status:", resp_status(resp), "\n")


brands_for_query <- dbGetQuery(pool,"SELECT 
dq.query_string,
MAX(fqh.date) as last_scored,
COUNT(*) as total_records
FROM fact_query_history fqh
JOIN dim_query dq ON dq.query_id = fqh.query_id
GROUP BY dq.query_string
ORDER BY last_scored DESC;")


result <- dbGetQuery(pool, "
    SELECT 
      job_type,
      status,
      date_trunc('day', created_at) as day,
      COUNT(*) as cnt
    FROM dim_job_queue
    WHERE created_at >= NOW() - INTERVAL '7 days'
    GROUP BY job_type, status, date_trunc('day', created_at)
    ORDER BY day DESC, job_type
  ")
  
  print(result)
  

  prompt_dates <- dbGetQuery(pool, "
  SELECT 
    dq.query_string,
    MAX(fqh.date) as last_scored,
    COUNT(*) as total_records
  FROM fact_query_history fqh
  JOIN dim_query dq ON dq.query_id = fqh.query_id
  GROUP BY dq.query_string
  ORDER BY last_scored DESC
")
  print(prompt_dates)
  
  recent_prompt_scores <- dbGetQuery(pool, "
  SELECT 
    dq.query_string,
    fqh.brand_id,
    b.brand_name,
    fqh.date,
    fqh.airr_score,
    fqh.presence_score,
    fqh.perception_score,
    fqh.prestige_score,
    fqh.persistence_score
  FROM fact_query_history fqh
  JOIN dim_query dq ON dq.query_id = fqh.query_id
  JOIN dim_brand b ON b.brand_id = fqh.brand_id
  WHERE fqh.date >= CURRENT_DATE - INTERVAL '3 days'
  ORDER BY fqh.date DESC, dq.query_string, b.brand_name
")
  
  
  nike_api <- "airr_rSunRiUxbinoA8ZHNjjCk2B5QwB8v9RRniAcPM02FC5tRanb"
  
  
  resp <- request("http://api.airrscore.com/api/v1/brand-scores") |>
    req_headers("X-API-Key" = nike_api) |>
    req_url_query(from = as.character(Sys.Date() - 30),
                  to   = as.character(Sys.Date())) |>
    req_perform()
  
  cat("Status:", resp_status(resp), "\n")
  data <- resp_body_json(resp, simplifyVector = TRUE)
  cat("Overall score rows:", nrow(as.data.frame(data$overall_scores)), "\n")
  cat("Persona score rows:", nrow(as.data.frame(data$persona_scores)), "\n")
  print(head(as.data.frame(data$overall_scores)))
  
  
  no <- as.data.frame(data$overall_scores)
  np <- as.data.frame(data$persona_scores)
  
  
  
  
  
  
  
  library(httr2)
  
  api_key  <- "YOUR_API_KEY"
  base_url <- "http://api.airrscore.com/api"
  
  # Brand scores
  resp <- request(paste0(base_url, "/v1/brand-scores")) |>
    req_headers("X-API-Key" = nike_api) |>
    req_url_query(from = "2024-01-01", to = Sys.Date()) |>
    req_perform()
  
  data          <- resp_body_json(resp, simplifyVector = TRUE)
  brand_scores  <- as.data.frame(data$overall_scores)
  persona_scores <- as.data.frame(data$persona_scores)
  
  # Prompt scores
  resp2 <- request(paste0(base_url, "/v1/prompt-scores")) |>
    req_headers("X-API-Key" = nike_api) |>
    req_url_query(from = "2024-01-01", to = Sys.Date()) |>
    req_perform()
  
  prompt_scores <- as.data.frame(
    resp_body_json(resp2, simplifyVector = TRUE)$overall_scores
  )
  
  # Filter to one brand
  resp3 <- request(paste0(base_url, "/v1/brand-scores")) |>
    req_headers("X-API-Key" = api_key) |>
    req_url_query(from = "2024-01-01",
                  to    = Sys.Date(),
                  brand = "Your Brand Name") |>
    req_perform()
  
  filtered <- as.data.frame(
    resp_body_json(resp3, simplifyVector = TRUE)$overall_scores
  )

  
  
  test_history <- data.frame(
    date          = seq.Date(Sys.Date() - 15, Sys.Date(), by = "day"),
    overall_score = c(0, 0, 0, 0, 0, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
  )
  
  result <- calculate_persistence_score("test", test_history)
  cat("Persistence score:", result$score, "\n")
  cat("CV:", result$coefficient_of_variation, "\n")
  
  
  
  
  
  
  
  # Guidance notice
  div(
    style = "background: rgba(102,126,234,0.06);
                 border: 1px solid rgba(102,126,234,0.2);
                 border-radius: 10px; padding: 14px 16px; margin-bottom: 20px;",
    div(
      style = "display: flex; align-items: flex-start; gap: 10px;",
      icon("bullseye",
           style = "color: #667eea; font-size: 15px;
                        flex-shrink: 0; margin-top: 2px;"),
      div(
        div(
          style = "font-weight: 600; font-size: 13px; color: #2d3748;
                       margin-bottom: 6px;",
          "Be as specific as possible"
        ),
        p(
          style = "font-size: 12px; color: #718096; margin: 0 0 8px;
                       line-height: 1.5;",
          "A reach that's too wide means your brand gets compared against ",
          "every competitor globally — making it harder to rank well and ",
          "producing less meaningful scores."
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 5px;",
          lapply(list(
            list(
              icon_name = "globe",
              col       = "#E74C3C",
              label     = "Global",
              bad       = TRUE,
              desc      = "Only use this if you genuinely compete worldwide"
            ),
            list(
              icon_name = "flag",
              col       = "#F39C12",
              label     = "National",
              bad       = FALSE,
              desc      = "Good if you operate across one country"
            ),
            list(
              icon_name = "map-marker-alt",
              col       = "#27AE60",
              label     = "Regional",
              bad       = FALSE,
              desc      = "Best for city or region-level brands"
            ),
            list(
              icon_name = "location-crosshairs",
              col       = "#27AE60",
              label     = "Near Me",
              bad       = FALSE,
              desc      = "Best for local businesses with a single location"
            )
          ), function(opt) {
            div(
              style = "display: flex; align-items: center; gap: 8px;
                           font-size: 12px;",
              icon(opt$icon_name,
                   style = paste0("color: ", opt$col, 
                                  "; font-size: 11px; flex-shrink: 0;",
                                  " width: 14px; text-align: center;")),
              tags$span(
                style = paste0("font-weight: 600; color: ", opt$col,
                               "; flex: 0 0 70px;"),
                opt$label
              ),
              tags$span(
                style = "color: #718096;",
                opt$desc
              ),
              if (opt$bad) {
                tags$span(
                  style = "margin-left: auto; font-size: 10px; font-weight: 600;
                               background: rgba(231,76,60,0.1); color: #E74C3C;
                               padding: 1px 7px; border-radius: 8px; flex-shrink: 0;",
                  "avoid unless global"
                )
              }
            )
          })
        )
      )
    )
  )
  
  
  
  
  
  
  # Run anytime to see current state
  dbGetQuery(pool, "
  SELECT 
    up.email,
    up.access_level,
    up.amount,
    up.purchase_date,
    up.is_active,
    u.login_id,
    u.onboarding_complete,
    ds.subscription_name as current_sub
  FROM user_purchases up
  LEFT JOIN dim_user u ON u.email = up.email
  LEFT JOIN fact_user_sub_level fus 
    ON fus.login_id = u.login_id
    AND fus.date_valid_from <= CURRENT_DATE
    AND fus.date_valid_to >= CURRENT_DATE
  LEFT JOIN dim_subscription ds
    ON ds.subscription_level_id = fus.subscription_level_id
  ORDER BY up.purchase_date DESC
")
  
  
  
  
  dbGetQuery(pool, "
  SELECT * FROM user_purchases 
  ORDER BY purchase_date DESC 
  LIMIT 5
")
  
  
  
  dbGetQuery(pool, "
  SELECT 
    u.email,
    ds.subscription_name,
    fus.date_valid_from,
    fus.date_valid_to
  FROM dim_user u
  JOIN fact_user_sub_level fus 
    ON fus.login_id = u.login_id
    AND fus.date_valid_to >= CURRENT_DATE
  JOIN dim_subscription ds 
    ON ds.subscription_level_id = fus.subscription_level_id
  ORDER BY fus.date_valid_from DESC
  LIMIT 5
")
  
  
  
  
  
  # Add Starter tier and update existing ones to match your new limits
  dbExecute(pool, "
  INSERT INTO dim_subscription 
    (subscription_level_id, subscription_name, 
     num_prompts_included, num_competitors_included)
  VALUES (2, 'Starter', 1, 5)
  ON CONFLICT (subscription_level_id) DO UPDATE SET
    subscription_name        = EXCLUDED.subscription_name,
    num_prompts_included     = EXCLUDED.num_prompts_included,
    num_competitors_included = EXCLUDED.num_competitors_included
")
  
  dbExecute(pool, "
  UPDATE dim_subscription SET
    subscription_name        = 'Pro',
    num_prompts_included     = 10,
    num_competitors_included = 10
  WHERE subscription_level_id = 3
")
  
  dbExecute(pool, "
  UPDATE dim_subscription SET
    subscription_name        = 'Enterprise',
    num_prompts_included     = 10,
    num_competitors_included = 10
  WHERE subscription_level_id = 4
")

# Add personas column if not already there
dbExecute(pool, "
  ALTER TABLE dim_subscription 
  ADD COLUMN IF NOT EXISTS num_personas_included INTEGER DEFAULT 0
  ")

dbExecute(pool, "
  UPDATE dim_subscription SET num_personas_included = 0 
  WHERE subscription_level_id = 1
  ")
dbExecute(pool, "
  UPDATE dim_subscription SET num_personas_included = 1 
  WHERE subscription_level_id = 2
  ")
dbExecute(pool, "
  UPDATE dim_subscription SET num_personas_included = 3 
  WHERE subscription_level_id = 3
  ")
dbExecute(pool, "
  UPDATE dim_subscription SET num_personas_included = 3 
  WHERE subscription_level_id = 4
  ")

# Confirm
dbGetQuery(pool, "SELECT * FROM dim_subscription ORDER BY subscription_level_id")
  
  
dbExecute(pool, "
  ALTER TABLE dim_subscription
  ADD COLUMN IF NOT EXISTS monthly_price_usd   NUMERIC,
  ADD COLUMN IF NOT EXISTS annual_price_usd    NUMERIC,
  ADD COLUMN IF NOT EXISTS stripe_monthly_link TEXT,
  ADD COLUMN IF NOT EXISTS stripe_annual_link  TEXT
")

# Free
dbExecute(pool, "
  UPDATE dim_subscription SET
    monthly_price_usd   = 0,
    annual_price_usd    = 0
  WHERE subscription_level_id = 1
")

# Starter
dbExecute(pool, "
  UPDATE dim_subscription SET
    monthly_price_usd   = 129,
    annual_price_usd    = 99,
    stripe_monthly_link = 'https://buy.stripe.com/dRmeVfep5cSv0zq9Q9gEg04',
    stripe_annual_link  = 'https://buy.stripe.com/00wbJ36WD2dRci86DXgEg03'
  WHERE subscription_level_id = 2
")

# Pro
dbExecute(pool, "
  UPDATE dim_subscription SET
    monthly_price_usd   = 449,
    annual_price_usd    = 349,
    stripe_monthly_link = 'https://buy.stripe.com/5kQ3cx3Kr8Cfci85zTgEg01',
    stripe_annual_link  = 'https://buy.stripe.com/28E9AV6WD8Cfaa01jDgEg00'
  WHERE subscription_level_id = 3
")

# Enterprise
dbExecute(pool, "
  UPDATE dim_subscription SET
    monthly_price_usd   = NULL,
    annual_price_usd    = NULL,
    stripe_monthly_link = 'https://airrscore.com/pricing',
    stripe_annual_link  = 'https://airrscore.com/pricing'
  WHERE subscription_level_id = 4
")
  
  
  




# Get Pro subscription level ID
pro_id <- dbGetQuery(pool, "
  SELECT subscription_level_id FROM dim_subscription
  WHERE subscription_name = 'Pro' LIMIT 1
")$subscription_level_id[1]

cat("Pro subscription_level_id:", pro_id, "\n")

# Login IDs to restore
login_ids <- c(45,46,51,52,53,54,58)


for (lid in login_ids) {
  dbExecute(pool, "
    INSERT INTO fact_user_sub_level
      (login_id, subscription_level_id, date_valid_from, date_valid_to)
    VALUES ($1, $2, CURRENT_DATE, '2099-12-31')
    ON CONFLICT DO NOTHING",
            params = list(lid, pro_id))
  cat("✓ login_id", lid, "→ Pro\n")
}

# Verify
result <- dbGetQuery(pool, "
  SELECT 
    u.login_id,
    u.email,
    ds.subscription_name,
    fus.date_valid_from,
    fus.date_valid_to
  FROM fact_user_sub_level fus
  JOIN dim_user u ON u.login_id = fus.login_id
  JOIN dim_subscription ds ON ds.subscription_level_id = fus.subscription_level_id
  WHERE fus.login_id = ANY($1)
    AND fus.date_valid_to >= CURRENT_DATE
  ORDER BY u.login_id",
                     params = list(login_ids))

print(result)
cat("\nTotal restored:", nrow(result), "of", length(login_ids), "\n")

dbExecute(pool, "
  ALTER TABLE fact_user_sub_level
  ADD COLUMN IF NOT EXISTS extra_personas_added INTEGER DEFAULT 0
")


dbGetQuery(pool, "SELECT * FROM dim_subscription ORDER BY subscription_level_id")

dbExecute(pool, "
  UPDATE dim_subscription 
  SET subscription_name        = 'Free',
      num_prompts_included     = 0,
      num_competitors_included = 0,
      num_personas_included    = 0,
      monthly_price_usd        = 0,
      annual_price_usd         = 0,
      stripe_monthly_link      = NULL,
      stripe_annual_link       = NULL
  WHERE subscription_level_id = 1
")

dbGetQuery(pool, "
  SELECT * FROM user_purchases 
  ORDER BY purchase_date DESC 
  LIMIT 5
")


dbGetQuery(pool, "
  SELECT 
    u.login_id,
    u.email,
    u.onboarding_complete,
    ds.subscription_name,
    fus.date_valid_from,
    fus.date_valid_to
  FROM dim_user u
  LEFT JOIN fact_user_sub_level fus 
    ON fus.login_id = u.login_id
    AND fus.date_valid_to >= CURRENT_DATE
  LEFT JOIN dim_subscription ds 
    ON ds.subscription_level_id = fus.subscription_level_id
  WHERE u.email = 'steve@syfter.com'
")

# Find the purchase that came in with the wrong email
dbGetQuery(pool, "
  SELECT * FROM user_purchases ORDER BY purchase_date DESC LIMIT 5
")

# Find their actual login_id by the email they registered with
user <- dbGetQuery(pool, "
  SELECT login_id, email FROM dim_user 
  WHERE email = 'sperlman1@gmail.com'
")

login_id <- user$login_id[1]

# Set their subscription correctly
sub_level_id <- dbGetQuery(pool, "
  SELECT subscription_level_id FROM dim_subscription
  WHERE subscription_name = 'Starter'  -- or Pro
  LIMIT 1
")$subscription_level_id[1]

dbExecute(pool, "
  UPDATE fact_user_sub_level
  SET date_valid_to = CURRENT_DATE - 1
  WHERE login_id = $1 AND date_valid_to >= CURRENT_DATE",
          params = list(login_id))

dbExecute(pool, "
  INSERT INTO fact_user_sub_level
    (login_id, subscription_level_id, date_valid_from, date_valid_to)
  VALUES ($1, $2, CURRENT_DATE, '2099-12-31')",
          params = list(login_id, sub_level_id))

cat("Done — login_id", login_id, "now has", 
    dbGetQuery(pool, "SELECT subscription_name FROM dim_subscription 
               WHERE subscription_level_id = $1",
               params = list(sub_level_id))$subscription_name, "\n")





dbExecute(pool, "
  ALTER TABLE dim_user 
  ADD COLUMN IF NOT EXISTS is_demo BOOLEAN DEFAULT FALSE
")

# Create a table mapping demo users to the accounts they can view
dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS dim_demo_accounts (
    id           SERIAL PRIMARY KEY,
    demo_login_id   INTEGER NOT NULL REFERENCES dim_user(login_id),
    target_login_id INTEGER NOT NULL REFERENCES dim_user(login_id),
    display_order   INTEGER DEFAULT 0,
    UNIQUE (demo_login_id, target_login_id)
  )
")

demo_login_id <- dbGetQuery(pool, "
  INSERT INTO dim_user (date_added, email, password_hash, 
                        onboarding_complete, is_demo)
  VALUES (CURRENT_DATE, 'demo@airrscore.com', $1, TRUE, TRUE)
  ON CONFLICT (email) DO UPDATE SET is_demo = TRUE
  RETURNING login_id",
                            params = list(digest::digest("Demo1234!", algo = "sha256")))$login_id

cat("Demo login_id:", demo_login_id, "\n")

# Give demo user Enterprise subscription so they see everything
ent_id <- dbGetQuery(pool, "
  SELECT subscription_level_id FROM dim_subscription
  WHERE subscription_name = 'Enterprise' LIMIT 1
")$subscription_level_id[1]

dbExecute(pool, "
  INSERT INTO fact_user_sub_level
    (login_id, subscription_level_id, date_valid_from, date_valid_to)
  VALUES ($1, $2, CURRENT_DATE, '2099-12-31')
  ON CONFLICT DO NOTHING",
          params = list(demo_login_id, ent_id))


# Link demo user to the accounts they should be able to view
# Replace these login_ids with real ones that have good data
demo_targets <- c(3, 37, 58)  # accounts with good scores to show off


for (i in seq_along(demo_targets)) {
  dbExecute(pool, "
    INSERT INTO dim_demo_accounts 
      (demo_login_id, target_login_id, display_order)
    VALUES ($1, $2, $3)
    ON CONFLICT DO NOTHING",
            params = list(demo_login_id, demo_targets[i], i))
}



dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS dim_password_reset_tokens (
    token_id     SERIAL PRIMARY KEY,
    login_id     INTEGER NOT NULL REFERENCES dim_user(login_id),
    token        VARCHAR(64) NOT NULL UNIQUE,
    created_at   TIMESTAMP NOT NULL DEFAULT NOW(),
    expires_at   TIMESTAMP NOT NULL,
    used         BOOLEAN NOT NULL DEFAULT FALSE
  )
")

dbExecute(pool, "
  CREATE INDEX IF NOT EXISTS idx_reset_tokens_token 
  ON dim_password_reset_tokens(token)
")

# Quick test
send_reset_email(
  to_email    = "alex@airrscore.com",
  reset_token = "test123",
  app_url     = "https://airr.airrscore.com/AiRR"
)

dbExecute(pool, "
  DELETE FROM dim_password_reset_tokens 
  WHERE token = 'lPzfbf20jxfJoYmZwtdTUB7QhIV9MZoZ5Fk9iMlx9Y3XvadZ'
")





dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS dim_brand_aliases (
    alias_id    SERIAL PRIMARY KEY,
    brand_id    INTEGER NOT NULL REFERENCES dim_brand(brand_id),
    alias_name  TEXT NOT NULL,
    date_added  DATE NOT NULL DEFAULT CURRENT_DATE,
    UNIQUE (brand_id, alias_name)
  )
")

# Index for fast lookup
dbExecute(pool, "
  CREATE INDEX IF NOT EXISTS idx_brand_aliases_brand_id 
  ON dim_brand_aliases(brand_id)
")



result <- dbGetQuery(pool, "
    SELECT *
    FROM dim_job_queue
  ")



brand_name = 'Living Group'
model          = "gpt-4o-mini"
industry       = 'creative agency'
brand_reach    = "global"
reach_country  = NULL
reach_region   = NULL
reach_postcode = NULL
brand_id       = 261   # new — for alias lookup
con            = con




full_air_score <- function(brand_name,
                           model          = "gpt-4o-mini",
                           industry       = NULL,
                           brand_reach    = "global",
                           reach_country  = NULL,
                           reach_region   = NULL,
                           reach_postcode = NULL,
                           brand_id       = NULL,   # new — for alias lookup
                           con            = NULL) { # new — for alias lookup
  
  get_data <- all_queries(brand_name,
                          industry       = industry,
                          model          = model,
                          brand_reach    = brand_reach,
                          reach_country  = reach_country,
                          reach_region   = reach_region,
                          reach_postcode = reach_postcode)
  
  # Build search names — main brand name + any aliases
  search_names <- if (!is.null(brand_id) && !is.null(con)) {
    get_brand_search_names(brand_id, brand_name, con)
  } else {
    brand_name  # fallback — no alias lookup
  }
  
  message(sprintf("  Presence search names: %s",
                  paste(search_names, collapse = ", ")))
  
  presence   <- calculate_presence_from_prompts(search_names,
                                                get_data$presence_data)
  perception <- calculate_perception_from_prompts(brand_name, get_data)
  prestige   <- calculate_prestige_from_prompts(brand_name, get_data)
  
  airr_scores            <- list()
  airr_scores$presence   <- presence
  airr_scores$perception <- perception
  airr_scores$prestige   <- prestige
  
  return(airr_scores)
}

# who publishes the best rankings of asset management firms based on brand and digital presence?

# Create first linked account for a user
create_linked_account(
  primary_email = "client@company.com",
  link_number   = 1,
  password      = "Welcome2024!",
  n_competitors = 10,
  n_prompts     = 10,
  n_personas    = 3
)
# Creates: link1.client@company.com

# Create second linked account with more slots
create_linked_account(
  primary_email = "client@company.com",
  link_number   = 2,
  password      = "Welcome2024!",
  n_competitors = 15,   # 5 more than Enterprise base of 10
  n_prompts     = 20,   # 10 more than Enterprise base
  n_personas    = 5
)
# Creates: link2.client@company.com

# View all linked accounts for a user
primary <- dbGetQuery(pool,
                      "SELECT login_id FROM dim_user WHERE email = 'client@company.com'")

get_linked_accounts(primary$login_id[1])



dbGetQuery(pool, "
  SELECT login_id, email, account_name, is_linked_account
  FROM dim_user
  WHERE is_linked_account = TRUE
")

# Get the current messy email
current <- dbGetQuery(pool, "
  SELECT login_id, email FROM dim_user WHERE is_linked_account = TRUE
")

# Build the clean version
clean_email <- gsub("\\s+", "", tolower(trimws(current$email[1])))

cat("Current:", repr(current$email[1]), "\n")
cat("Clean:  ", clean_email, "\n")

# Update it
dbExecute(pool, "
  UPDATE dim_user
  SET email = $1
  WHERE login_id = $2",
          params = list(clean_email, current$login_id[1]))

# Verify
dbGetQuery(pool, "
  SELECT login_id, email, account_name
  FROM dim_user
  WHERE is_linked_account = TRUE
")





resp <- httr2::request("https://api.openai.com/v1/chat/completions") |>
  httr2::req_headers(
    Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
    `Content-Type` = "application/json"
  ) |>
  httr2::req_body_json(list(
    model = "gpt-4o-mini",          # cheap model just to probe headers
    messages = list(
      list(role = "user", content = "hi")
    ),
    max_tokens = 1L                  # minimize cost/tokens used
  )) |>
  httr2::req_perform()

headers <- httr2::resp_headers(resp)

cat("Requests limit:     ", headers$`x-ratelimit-limit-requests`, "\n")
cat("Requests remaining: ", headers$`x-ratelimit-remaining-requests`, "\n")
cat("Tokens limit:       ", headers$`x-ratelimit-limit-tokens`, "\n")
cat("Tokens remaining:   ", headers$`x-ratelimit-remaining-tokens`, "\n")
cat("Reset requests at:  ", headers$`x-ratelimit-reset-requests`, "\n")
cat("Reset tokens at:    ", headers$`x-ratelimit-reset-tokens`, "\n")



# Catch the error and inspect it
resp <- tryCatch(
  httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = "gpt-4o-mini",
      messages = list(list(role = "user", content = "hi")),
      max_tokens = 1L
    )) |>
    httr2::req_error(is_error = \(resp) FALSE) |>  # don't throw on 4xx/5xx
    httr2::req_perform(),
  error = function(e) e
)

# Now inspect regardless of 429
headers <- httr2::resp_headers(resp)

cat("Status:             ", httr2::resp_status(resp), "\n")
cat("Requests limit:     ", headers$`x-ratelimit-limit-requests`, "\n")
cat("Requests remaining: ", headers$`x-ratelimit-remaining-requests`, "\n")
cat("Tokens limit:       ", headers$`x-ratelimit-limit-tokens`, "\n")
cat("Tokens remaining:   ", headers$`x-ratelimit-remaining-tokens`, "\n")
cat("Reset requests at:  ", headers$`x-ratelimit-reset-requests`, "\n")
cat("Reset tokens at:    ", headers$`x-ratelimit-reset-tokens`, "\n")

# Also print the error body - tells you WHAT limit you hit
cat("\nError body:\n")
print(httr2::resp_body_json(resp))


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

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

cat('Connected OK\n')
res <- DBI::dbGetQuery(con, 'SELECT COUNT(*) FROM dim_user')
cat('User count:', res[[1]], '\n')
DBI::dbDisconnect(con)




# Set accounts 51, 52, 53, 54 as linked to master account 42

# Step 1: Check what we're working with first
dbGetQuery(pool, "
  SELECT login_id, email, account_name, 
         is_linked_account, primary_login_id
  FROM dim_user
  WHERE login_id IN (42, 51, 52, 53, 54)
  ORDER BY login_id
")

# Step 2: Get master account details
master <- dbGetQuery(pool, "
  SELECT login_id, email, account_name FROM dim_user WHERE login_id = 42
")
cat("Master account:", master$email, "\n")

# Step 3: Link the accounts
linked_ids <- c(51, 52, 53, 54)

for (lid in linked_ids) {
  dbExecute(pool, "
    UPDATE dim_user
    SET is_linked_account  = TRUE,
        primary_login_id   = 42
    WHERE login_id = $1",
            params = list(lid))
  cat(sprintf("✓ login_id %d linked to master (42)\n", lid))
}

# Step 4: Verify
dbGetQuery(pool, "
  SELECT 
    u.login_id,
    u.email,
    COALESCE(u.account_name, b.brand_name, u.email) AS display_name,
    u.is_linked_account,
    u.primary_login_id
  FROM dim_user u
  LEFT JOIN fact_user_brands_tracked ubt
    ON ubt.login_id = u.login_id
    AND ubt.main_brand_flag = TRUE
    AND ubt.date_valid_from <= CURRENT_DATE
    AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  LEFT JOIN dim_brand b ON b.brand_id = ubt.brand_id
  WHERE u.login_id IN (42, 51, 52, 53, 54)
  ORDER BY u.login_id
")


demo_login_id <- 54
demo_brand <- 'Burton'

generate_and_email_demo_report(
  login_id   = 54,
  brand_name = "Burton",
  to_email   = c("alex@airrscore.com", "steven@airrscore.com")
)

generate_and_email_demo_report(
  login_id   = 67,   # onboarding account login_id
  brand_name = "Veoci",
  to_email   = "alex@airrscore.com"
)