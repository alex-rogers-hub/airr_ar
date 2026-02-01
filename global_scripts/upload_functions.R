
AIRR_WEIGHTS <- list(
  presence = 0.25,
  perception = 0.3,
  prestige = 0.25,
  persistence = 0.2
)

add_customer <- function(con, customer_name) {
  query <- "
    INSERT INTO dim_customer (customer_name)
    VALUES ($1)
    RETURNING customer_id;
  "
  result <- dbGetQuery(con, query, params = list(customer_name))
  return(result$customer_id)
}

upload_daily_refresh <- function(brand_name, input_data, run_date){
  
  # Start transaction for data integrity
  dbBegin(con)
  
  tryCatch({
    
    score_date <- run_date
    
    air_scores <- input_data
    
    cust_id <- dbGetQuery(con,
                          paste0("select customer_id
                                from dim_customer
                                where lower(customer_name) = lower('",
                                 brand_name,"');"))
    
    if(nrow(cust_id) == 0){
      cust_id_ret <- add_customer(con,brand_name)
    } else {
      cust_id_ret <- cust_id$customer_id
    }
    
    # ==================== INSERT PRESENCE DATA ====================
    presence <- air_scores$presence
    
    dbExecute(con, "
      INSERT INTO fact_presence_history (
        customer_id,
        date,
        overall_score,
        simple_mention_rate,
        total_responses,
        responses_with_mentions,
        interpretation,
        stability
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      ON CONFLICT (customer_id, date) 
      DO UPDATE SET
        overall_score = EXCLUDED.overall_score,
        simple_mention_rate = EXCLUDED.simple_mention_rate,
        total_responses = EXCLUDED.total_responses,
        responses_with_mentions = EXCLUDED.responses_with_mentions,
        interpretation = EXCLUDED.interpretation,
        stability = EXCLUDED.stability;
    ", params = list(
      cust_id_ret,
      score_date,
      presence$overall_score,
      presence$simple_mention_rate,
      presence$total_responses,
      presence$responses_with_mentions,
      presence$interpretation,
      presence$stability
    ))
    
    message(sprintf("✓ Inserted presence data for %s (ID: %d)", brand_name, cust_id_ret))
    
    # ==================== INSERT PERCEPTION DATA ====================
    perception <- air_scores$perception
    
    dbExecute(con, "
      INSERT INTO fact_perception_history (
        customer_id,
        date,
        perception_score,
        perception_accuracy_score,
        perception_sentiment_score,
        prestige_accuracy_interpretation
      ) VALUES ($1, $2, $3, $4, $5, $6)
      ON CONFLICT (customer_id, date) 
      DO UPDATE SET
        perception_score = EXCLUDED.perception_score,
        perception_accuracy_score = EXCLUDED.perception_accuracy_score,
        perception_sentiment_score = EXCLUDED.perception_sentiment_score,
        prestige_accuracy_interpretation = EXCLUDED.prestige_accuracy_interpretation;
    ", params = list(
      cust_id_ret,
      score_date,
      perception$perception_score,
      perception$perception_accuracy_score,
      perception$perception_sentiment_score,
      perception$prestige_accuracy_interpretation
    ))
    
    message(sprintf("✓ Inserted perception data for %s (ID: %d)", brand_name, cust_id_ret))
    
    # ==================== INSERT PRESTIGE DATA ====================
    prestige <- air_scores$prestige
    
    dbExecute(con, "
      INSERT INTO fact_prestige_history (
        customer_id,
        date,
        prestige_score,
        prestige_rank_score,
        prestige_rank_comps_brands,
        prestige_authority_score,
        prestige_leadership_score
      ) VALUES ($1, $2, $3, $4, $5, $6, $7)
      ON CONFLICT (customer_id, date) 
      DO UPDATE SET
        prestige_score = EXCLUDED.prestige_score,
        prestige_rank_score = EXCLUDED.prestige_rank_score,
        prestige_rank_comps_brands = EXCLUDED.prestige_rank_comps_brands,
        prestige_authority_score = EXCLUDED.prestige_authority_score,
        prestige_leadership_score = EXCLUDED.prestige_leadership_score;
    ", params = list(
      cust_id_ret,
      score_date,
      prestige$prestige_score,
      prestige$prestige_rank_score,
      paste(prestige$prestige_rank_comps$brand, collapse = " | "),
      prestige$prestige_authority_score,
      prestige$prestige_leadership_score
    ))
    message(sprintf("✓ Inserted prestige data for %s (ID: %d)", brand_name, cust_id_ret))
    
    # Commit transaction
    dbCommit(con)
    message(sprintf("✓✓ Successfully inserted all AIR scores for %s", brand_name))
    
    return(TRUE)
  }, error = function(e) {
    # Rollback on error
    dbRollback(con)
    warning(sprintf("✗ Error inserting data for %s: %s", brand_name, e$message))
    return(FALSE)
  })
  
}

calc_daily_persistance <- function(brand_name, run_date){
  persistence <- calculate_daily_persistence(brand_name)
  score_date <- run_date
  cust_id <- dbGetQuery(con,
                        paste0("select customer_id
                                from dim_customer
                                where lower(customer_name) = lower('",
                               brand_name,"');"))
  
  if(nrow(cust_id) == 0){
    cust_id_ret <- add_customer(con,brand_name)
  } else {
    cust_id_ret <- cust_id$customer_id
  }
  
  # Start transaction for data integrity
  dbBegin(con)
  
  dbExecute(con, "
      INSERT INTO fact_persistence_history (
        customer_id,
        date,
        persistence_score,
        coefficient_of_variation,
        interpretation,
        daily_perc_change
      ) VALUES ($1, $2, $3, $4, $5, $6)
      ON CONFLICT (customer_id, date) 
      DO UPDATE SET
        persistence_score = EXCLUDED.persistence_score,
        coefficient_of_variation = EXCLUDED.coefficient_of_variation,
        interpretation = EXCLUDED.interpretation,
        daily_perc_change = EXCLUDED.daily_perc_change;
    ", params = list(
      cust_id_ret,
      score_date,
      persistence$score,
      persistence$coefficient_of_variation,
      persistence$interpretation,
      persistence$daily_perc_change
    ))
  message(sprintf("✓ Inserted prestige data for %s (ID: %d)", brand_name, cust_id_ret))
  # Commit transaction
  dbCommit(con)
}

calc_daily_airr <- function(brand_name, run_date){
  score_date <- run_date
  cust_id <- dbGetQuery(con,
                        paste0("select customer_id
                                from dim_customer
                                where lower(customer_name) = lower('",
                               brand_name,"');"))
  
  if(nrow(cust_id) == 0){
    cust_id_ret <- add_customer(con,brand_name)
  } else {
    cust_id_ret <- cust_id$customer_id
  }
  
  persistence <- dbGetQuery(con,
                        paste0("select persistence_score
                                from fact_persistence_history
                                where customer_id = ",
                                cust_id_ret," and date = '",
                               score_date,"';"))$persistence_score
  
  presence <- dbGetQuery(con,
                            paste0("select overall_score
                                from fact_presence_history
                                where customer_id = ",
                                   cust_id_ret," and date = '",
                                   score_date,"';"))$overall_score
  
  perception <- dbGetQuery(con,
                            paste0("select perception_score
                                from fact_perception_history
                                where customer_id = ",
                                   cust_id_ret," and date = '",
                                   score_date,"';"))$perception_score
  
  prestige <- dbGetQuery(con,
                           paste0("select prestige_score
                                from fact_prestige_history
                                where customer_id = ",
                                  cust_id_ret," and date = '",
                                  score_date,"';"))$prestige_score
  
  airr_score <- perception*AIRR_WEIGHTS$perception + presence*AIRR_WEIGHTS$presence +
    prestige*AIRR_WEIGHTS$prestige + persistence*AIRR_WEIGHTS$persistence
  
  # Start transaction for data integrity
  dbBegin(con)
  
  dbExecute(con, "
      INSERT INTO fact_airr_history (
        customer_id,
        date,
        airr_score
      ) VALUES ($1, $2, $3)
      ON CONFLICT (customer_id, date) 
      DO UPDATE SET
        airr_score = EXCLUDED.airr_score;
    ", params = list(
      cust_id_ret,
      score_date,
      airr_score
    ))
  message(sprintf("✓ Inserted airr_score data for %s (ID: %d)", brand_name, cust_id_ret))
  # Commit transaction
  dbCommit(con)
}

# ttt <- dbGetQuery(con,'select * from fact_prestige_history')

daily_refresh_loop <- function(model = "gpt-4o-mini"){
  customer_list <- dbGetQuery(con,'select * from dim_customer')
  
  cust_names <- customer_list$customer_name
  for(i in cust_names){
    airr_scores <- full_air_score(i, model)
    upload_daily_refresh(i,
                         airr_scores,
                         as.Date(Sys.Date()))
    calc_daily_persistance(i,
                           Sys.Date())
    calc_daily_airr(i,
                    Sys.Date())
    
  }
}
# daily_refresh_loop("gpt-4o")

