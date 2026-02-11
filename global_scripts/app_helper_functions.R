
# Helper function to fetch user dashboard data
get_user_metrics <- function(customer_id) {
  queryDC <- "SELECT customer_id,
  customer_name,
  date_added
  from dim_customer
  WHERE customer_id = $1"
  queryA <- "SELECT * from fact_presence_history
            WHERE customer_id = $1"
  queryB <- "SELECT * from fact_perception_history
            WHERE customer_id = $1"
  queryC <- "SELECT * from fact_prestige_history
            WHERE customer_id = $1"
  queryD <- "SELECT * from fact_persistence_history
            WHERE customer_id = $1"
  queryE <- "SELECT * from fact_airr_history
            WHERE customer_id = $1"
  
  user_metrics <- list(
    cust = dbGetQuery(pool, queryDC, params = list(customer_id)),
    presence = dbGetQuery(pool, queryA, params = list(customer_id)),
    perception = dbGetQuery(pool, queryB, params = list(customer_id)),
    prestige = dbGetQuery(pool, queryC, params = list(customer_id)),
    persistence = dbGetQuery(pool, queryD, params = list(customer_id)),
    airr = dbGetQuery(pool, queryE, params = list(customer_id))
  )
  return(user_metrics)
}

# Function to get latest score with specific column name
get_latest_score <- function(df, score_col) {
  if (nrow(df) == 0) return(0)
  
  # If there's a date column, get the most recent
  # date_cols <- names(df)[sapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt")))]
  latest_idx <- which.max(df$date)
  return(as.numeric(df[[score_col]][latest_idx]))
}

addPrompt <- function(prompt_string, customer_id) {
  # Get a connection from the pool
  con <- poolCheckout(pool)
  
  tryCatch({
    # Start transaction
    # dbBegin(con)
    
    # Insert query and get ID
    query <- "INSERT INTO dim_query (query_string) 
              VALUES ($1) 
              RETURNING query_id"
    
    result <- dbGetQuery(con, query, params = list(prompt_string))
    
    # Insert into junction table
    dbExecute(con, "
      INSERT INTO dim_cust_query (
        customer_id,
        query_id,
        date_added
      ) VALUES ($1, $2, $3)
      ON CONFLICT (customer_id, query_id)
      DO UPDATE SET
        date_added = EXCLUDED.date_added
    ", params = list(
      customer_id,
      result$query_id,
      Sys.time()  # Changed to Sys.time() for timestamp
    ))
    
    # Commit transaction
    # dbCommit(con)
    
    # calculate the score for this prompt
    # dbBegin(con)
    
    create_prompt_airr(customer_id,
                       prompt_string,
                       result$query_id)
    
    # dbCommit(con)
    
    # Return success
    return(result$query_id)
    
  }, error = function(e) {
    # Rollback on error
    # dbRollback(con)
    warning(paste("Error in addPrompt:", e$message))
    return(NULL)
    
  }, finally = {
    # Always return connection to pool
    poolReturn(con)
  })
}

# addPrompt('Who makes the best running shoes?',1)
# ttt <- dbGetQuery(con,'select * from dim_cust_query')
# ttt2 <- dbGetQuery(con,'select * from dim_query')

get_customer_queries <- function(customer_id) {
  query <- "SELECT dcq.customer_id, dcq.query_id, dcq.date_added,
                   dq.query_string
            FROM dim_cust_query dcq
            JOIN dim_query dq ON dcq.query_id = dq.query_id
            WHERE dcq.customer_id = $1
            ORDER BY dcq.date_added DESC"
  
  dbGetQuery(pool, query, params = list(customer_id))
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
get_latest_query_scores <- function(query_id, customer_id) {
  query <- "SELECT query_id, date, airr_score, 
                   presence_score, perception_score, 
                   prestige_score, persistence_score
            FROM fact_query_history
            WHERE query_id = $1
            AND customer_id = $2
            ORDER BY date DESC
            LIMIT 1"
  
  result <- dbGetQuery(pool, query, params = list(query_id, customer_id))
  
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
    
    cat("Step 1: Getting customers\n")
    all_customers <- dbGetQuery(
      con,
      "SELECT DISTINCT customer_id FROM dim_customer"
    )$customer_id
    cat("Found", length(all_customers), "customers\n")
    
    cat("Step 2: Starting transaction\n")
    dbBegin(con)
    
    cat("Step 3: Inserting query\n")
    query <- "INSERT INTO dim_query (query_string) 
              VALUES ($1) 
              RETURNING query_id"
    
    result <- dbGetQuery(con, query, params = list(prompt_string))
    query_id <- result$query_id
    cat("Query inserted with ID:", query_id, "\n")
    
    cat("Step 4: Building junction table values\n")
    current_date <- Sys.Date()
    values_strings <- paste0(
      "(",
      all_customers,
      ", ",
      query_id,
      ", '",
      current_date,
      "')"
    )
    
    cat("Step 5: Inserting into junction table\n")
    query <- paste0("
      INSERT INTO dim_cust_query (
        customer_id,
        query_id,
        date_added
      )
      VALUES ",
                    paste(values_strings, collapse = ", "),
                    " ON CONFLICT (customer_id, query_id)
      DO UPDATE SET
        date_added = EXCLUDED.date_added
    ")
    
    dbExecute(con, query)
    cat("Junction table updated\n")
    
    cat("Step 6: Committing transaction\n")
    dbCommit(con)
    
    cat("Step 7: About to call create_prompt_airr_multiple\n")
    create_prompt_airr_multiple(all_customers, prompt_string, query_id)
    cat("Step 8: Finished create_prompt_airr_multiple\n")
    
    cat("=== addPromptForEveryone completed successfully ===\n")
    return(query_id)
    
  }, error = function(e) {
    cat("=== ERROR in addPromptForEveryone ===\n")
    cat("Error message:", e$message, "\n")
    cat("Error occurred at step above\n")
    
    tryCatch(dbRollback(con), error = function(e2) {
      cat("Could not rollback:", e2$message, "\n")
    })
    
    stop(e)
  })
}


