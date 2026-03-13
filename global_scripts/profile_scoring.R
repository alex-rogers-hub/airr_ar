score_profile <- function(con, login_id, profile_id, model = "gpt-4o-mini") {
  
  profile <- dbGetQuery(con,
                        "SELECT profile_name, profile_descriptor 
     FROM dim_customer_profile WHERE profile_id = $1",
                        params = list(profile_id))
  
  if (nrow(profile) == 0) stop(paste("Profile not found:", profile_id))
  
  descriptor <- profile$profile_descriptor[1]
  prefix     <- paste0(descriptor, " ")
  
  message(sprintf("=== Scoring profile: %s ===", profile$profile_name[1]))
  
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
  
  if (nrow(brands) == 0) {
    message("No brands found for user")
    return(invisible(NULL))
  }
  
  queries <- dbGetQuery(con, "
    SELECT dq.query_id, dq.query_string
    FROM fact_user_queries_tracked uqt
    JOIN dim_query dq ON dq.query_id = uqt.query_id
    WHERE uqt.login_id = $1
      AND uqt.date_valid_from <= CURRENT_DATE
      AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
  ", params = list(login_id))
  
  for (b in seq_len(nrow(brands))) {
    bid        <- brands$brand_id[b]
    brand_name <- brands$brand_name[b]
    reach      <- brands$brand_reach[b] %||% "global"
    country    <- brands$reach_country[b]
    region     <- brands$reach_region[b]
    postcode   <- brands$reach_postcode[b]
    
    if (is.na(country))  country  <- NULL
    if (is.na(region))   region   <- NULL
    if (is.na(postcode)) postcode <- NULL
    
    message(sprintf("  Scoring brand: %s (ID: %d)", brand_name, bid))
    
    tryCatch({
      score_profile_brand(
        con            = con,
        brand_id       = bid,
        brand_name     = brand_name,
        profile_id     = profile_id,
        login_id       = login_id,       # <-- was missing
        prefix         = prefix,
        brand_reach    = reach,
        reach_country  = country,
        reach_region   = region,
        reach_postcode = postcode,
        model          = model
      )
    }, error = function(e) {
      warning(sprintf("Failed brand %s: %s", brand_name, e$message))
      message(sprintf("  ✗ Brand scoring error for %s: %s", brand_name, e$message))
    })
    
    if (nrow(queries) > 0) {
      for (q in seq_len(nrow(queries))) {
        qid     <- queries$query_id[q]
        qstring <- queries$query_string[q]
        
        tryCatch({
          score_profile_prompt(
            con            = con,
            brand_id       = bid,
            brand_name     = brand_name,
            profile_id     = profile_id,
            login_id       = login_id,   # <-- was missing
            query_id       = qid,
            query_string   = qstring,
            prefix         = prefix,
            brand_reach    = reach,
            reach_country  = country,
            reach_postcode = postcode,
            model          = model
          )
        }, error = function(e) {
          warning(sprintf("Failed prompt %d for brand %s: %s", qid, brand_name, e$message))
          message(sprintf("  ✗ Prompt scoring error (query %d, brand %s): %s",
                          qid, brand_name, e$message))
        })
      }
    }
  }
  
  message(sprintf("=== Profile scoring complete: %s ===", profile$profile_name[1]))
}


score_profile_brand <- function(con, brand_id, brand_name, profile_id,
                                login_id,              # <-- added
                                prefix,
                                brand_reach    = "global",
                                reach_country  = NULL,
                                reach_region   = NULL,
                                reach_postcode = NULL,
                                model          = "gpt-4o-mini") {
  
  reach_context <- build_reach_context(brand_reach, reach_country, reach_region, reach_postcode)
  reach_str     <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
  near_me_str   <- build_near_me_suffix(brand_reach, reach_country, reach_postcode)
  
  reps <- 10
  
  presence_prompts <- c(
    rep(paste0(prefix, "What is the best brand for me in this category", reach_str, "?"), reps),
    rep(paste0(prefix, "Which brand would you recommend for someone like me", reach_str, "?"), reps),
    rep(paste0(prefix, "What brand should I choose", reach_str, "?"), reps),
    rep(paste0(prefix, "Which company is most relevant to my needs", reach_str, "?"), reps),
    rep(paste0(prefix, "What are the top brands for people like me",
               if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps)
  )
  
  perception_prompts <- c(
    rep(paste0(prefix, "Tell me about ", brand_name, reach_str), reps),
    rep(paste0(prefix, "Is ", brand_name, " a good choice for someone like me", reach_str, "?"), reps),
    rep(paste0(prefix, "What do people like me think of ", brand_name, reach_str, "?"), reps)
  )
  
  all_prompts <- c(presence_prompts, perception_prompts)
  
  responses <- ask_chatgpt_async(
    prompts     = all_prompts,
    model       = model,
    temperature = 0.3
  )
  
  idx <- 0
  grab <- function(n) {
    result <- responses[(idx + 1):(idx + n)]
    idx <<- idx + n
    result
  }
  
  pres1 <- grab(reps); pres2 <- grab(reps); pres3 <- grab(reps)
  pres4 <- grab(reps); pres5 <- grab(reps)
  perc1 <- grab(reps); perc2 <- grab(reps); perc3 <- grab(reps)
  
  all_presence_responses <- data.frame(
    prompt    = rep("profile_presence", reps * 5),
    responses = sanitise_text(unlist(c(pres1, pres2, pres3, pres4, pres5))),
    stringsAsFactors = FALSE
  )
  
  presence_score <- presence_prompt_calc(brand_name, all_presence_responses)
  
  all_perception_text <- sanitise_text(unlist(c(perc1, perc2, perc3)))
  sentiment_result    <- analyze_sentiment_weighted(all_perception_text)
  perception_score    <- sentiment_result$score
  
  # FIX: pass con and login_id correctly
  prestige_score <- calculate_prestige_from_prompts_sep(
    brand_name     = brand_name,
    brand_id       = brand_id,
    login_id       = login_id,
    rel_responses  = all_presence_responses,
    db_con            = con
  )
  
  presence_history <- dbGetQuery(con,
                                 "SELECT date, presence_score FROM fact_profile_brand_history
     WHERE profile_id = $1 AND brand_id = $2
     ORDER BY date",
                                 params = list(profile_id, brand_id))
  
  persistence_score <- if (nrow(presence_history) < 5) {
    presence_score
  } else {
    calculate_daily_persistence_sep(presence_history)
  }
  
  airr_score <- presence_score    * AIRR_WEIGHTS$presence +
    perception_score  * AIRR_WEIGHTS$perception +
    prestige_score    * AIRR_WEIGHTS$prestige +
    persistence_score * AIRR_WEIGHTS$persistence
  
  dbExecute(con, "
    INSERT INTO fact_profile_brand_history
      (profile_id, brand_id, date, presence_score, perception_score,
       prestige_score, persistence_score, airr_score)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    ON CONFLICT (profile_id, brand_id, date)
    DO UPDATE SET
      presence_score    = EXCLUDED.presence_score,
      perception_score  = EXCLUDED.perception_score,
      prestige_score    = EXCLUDED.prestige_score,
      persistence_score = EXCLUDED.persistence_score,
      airr_score        = EXCLUDED.airr_score;
  ", params = list(
    profile_id, brand_id, Sys.Date(),
    presence_score, perception_score, prestige_score, persistence_score, airr_score
  ))
  
  message(sprintf("    ✓ Brand %s scored for profile %d", brand_name, profile_id))
}


score_profile_prompt <- function(con, brand_id, brand_name, profile_id,
                                 login_id,              # <-- added
                                 query_id, query_string, prefix,
                                 brand_reach    = "global",
                                 reach_country  = NULL,
                                 reach_postcode = NULL,
                                 model          = "gpt-4o-mini") {
  
  query_resolved  <- inject_near_me_location(query_string, brand_reach, reach_country, reach_postcode)
  profiled_prompt <- paste0(prefix, query_resolved)
  
  query_list    <- rep(profiled_prompt, 10)
  rel_responses <- prompt_queries(query_list, model = model)
  
  presence_score   <- presence_prompt_calc(brand_name, rel_responses)
  
  # FIX: pass con and login_id correctly
  prestige_score   <- calculate_prestige_from_prompts_sep(
    brand_name    = brand_name,
    brand_id      = brand_id,
    login_id      = login_id,
    rel_responses = rel_responses,
    db_con           = con
  )
  
  perception_score <- calculate_perception_from_prompts_sep(
    brand_name, brand_id, rel_responses
  )
  
  presence_history <- dbGetQuery(con,
                                 "SELECT date, presence_score FROM fact_profile_query_history
     WHERE profile_id = $1 AND brand_id = $2 AND query_id = $3
     ORDER BY date",
                                 params = list(profile_id, brand_id, query_id))
  
  persistence_score <- if (nrow(presence_history) < 5) {
    presence_score
  } else {
    calculate_daily_persistence_sep(presence_history)
  }
  
  airr_score <- presence_score    * AIRR_WEIGHTS$presence +
    perception_score  * AIRR_WEIGHTS$perception +
    prestige_score    * AIRR_WEIGHTS$prestige +
    persistence_score * AIRR_WEIGHTS$persistence
  
  dbExecute(con, "
    INSERT INTO fact_profile_query_history
      (profile_id, brand_id, query_id, date, presence_score, perception_score,
       prestige_score, persistence_score, airr_score)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
    ON CONFLICT (profile_id, brand_id, query_id, date)
    DO UPDATE SET
      presence_score    = EXCLUDED.presence_score,
      perception_score  = EXCLUDED.perception_score,
      prestige_score    = EXCLUDED.prestige_score,
      persistence_score = EXCLUDED.persistence_score,
      airr_score        = EXCLUDED.airr_score;
  ", params = list(
    profile_id, brand_id, query_id, Sys.Date(),
    presence_score, perception_score, prestige_score, persistence_score, airr_score
  ))
  
  message(sprintf("    ✓ Prompt %d scored for brand %s, profile %d",
                  query_id, brand_name, profile_id))
}