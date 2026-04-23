# profile_scoring.R
# Batched version — one OpenAI batch submission per profile
# instead of one per brand per prompt

# ============================================================
# score_profile — main entry point (unchanged signature)
# ============================================================

score_profile <- function(con, login_id, profile_id,
                          model     = "gpt-4o-mini",
                          use_batch = FALSE) {
  
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
  
  # ── Use batched version when use_batch = TRUE ─────────────────
  if (use_batch) {
    score_profile_batched(
      con        = con,
      login_id   = login_id,
      profile_id = profile_id,
      prefix     = prefix,
      brands     = brands,
      queries    = queries,
      model      = model
    )
  } else {
    # Original sequential path — unchanged, used for interactive jobs
    score_profile_sequential(
      con        = con,
      login_id   = login_id,
      profile_id = profile_id,
      prefix     = prefix,
      brands     = brands,
      queries    = queries,
      model      = model
    )
  }
  
  message(sprintf("=== Profile scoring complete: %s ===",
                  profile$profile_name[1]))
}

# ============================================================
# score_profile_batched
# Builds ALL prompts for ALL brands + ALL queries in one pass,
# submits a single OpenAI batch, then scores everything from results
# ============================================================

score_profile_batched <- function(con, login_id, profile_id, prefix,
                                  brands, queries, model) {
  
  reps <- 10
  
  # ── Phase 1: Build prompt map ──────────────────────────────────
  # Map: custom_id -> prompt text
  # We'll name them so we can route results back
  # Format: b{brand_idx}_brand_p{001..080}   — brand scoring prompts
  #         b{brand_idx}_q{query_idx}_p{001..010} — query scoring prompts
  
  all_prompt_map  <- list()
  prompt_index    <- list()  # tracks which ids belong to which brand/query
  
  for (b in seq_len(nrow(brands))) {
    bid        <- brands$brand_id[b]
    brand_name <- brands$brand_name[b]
    reach      <- brands$brand_reach[b] %||% "global"
    country    <- if (is.na(brands$reach_country[b]))  NULL else brands$reach_country[b]
    region     <- if (is.na(brands$reach_region[b]))   NULL else brands$reach_region[b]
    postcode   <- if (is.na(brands$reach_postcode[b])) NULL else brands$reach_postcode[b]
    
    reach_context <- build_reach_context(reach, country, region, postcode)
    reach_str     <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
    near_me_str   <- build_near_me_suffix(reach, country, postcode)
    loc_str       <- if (nzchar(near_me_str)) near_me_str else reach_str
    
    # Get industry for this brand
    industry <- tryCatch({
      result <- dbGetQuery(con,
                           "SELECT industry FROM fact_user_brands_tracked
         WHERE login_id = $1 AND brand_id = $2
           AND date_valid_from <= CURRENT_DATE
           AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
         LIMIT 1",
                           params = list(login_id, bid))
      if (nrow(result) > 0 && !is.na(result$industry[1])) result$industry[1] else NULL
    }, error = function(e) NULL)
    
    industry_str <- if (!is.null(industry) && nzchar(industry)) industry else "their industry"
    
    # Brand-level prompts (80 total: 50 presence + 30 perception)
    brand_prompts <- c(
      rep(paste0(prefix, 'What are the best brands in the ', industry_str,
                 ' industry', loc_str, '?'), reps),
      rep(paste0(prefix, 'What are the top brands in ', industry_str,
                 loc_str, '?'), reps),
      rep(paste0(prefix, 'Which companies lead the ', industry_str,
                 ' industry', loc_str, '?'), reps),
      rep(paste0(prefix, 'Recommend some ', industry_str,
                 ' brands', loc_str), reps),
      rep(paste0(prefix, 'What are popular ', industry_str,
                 ' companies', loc_str, '?'), reps),
      rep(paste0(prefix, 'Tell me about ', brand_name, reach_str), reps),
      rep(paste0(prefix, 'Is ', brand_name,
                 ' a good choice for someone like me', reach_str, '?'), reps),
      rep(paste0(prefix, 'What do people like me think of ',
                 brand_name, reach_str, '?'), reps)
    )
    
    brand_ids <- sprintf("b%d_brand_p%03d", b, seq_along(brand_prompts))
    names(brand_prompts) <- brand_ids
    all_prompt_map <- c(all_prompt_map, as.list(brand_prompts))
    
    prompt_index[[sprintf("b%d_brand", b)]] <- list(
      type       = "brand",
      brand_idx  = b,
      brand_id   = bid,
      brand_name = brand_name,
      ids        = brand_ids
    )
    
    # Query-level prompts (10 per query)
    if (nrow(queries) > 0) {
      for (q in seq_len(nrow(queries))) {
        qid     <- queries$query_id[q]
        qstring <- queries$query_string[q]
        
        query_resolved  <- inject_near_me_location(
          qstring, reach, country, region, postcode)
        profiled_prompt <- paste0(prefix, query_resolved)
        
        query_prompts <- rep(profiled_prompt, reps)
        query_ids     <- sprintf("b%d_q%d_p%03d", b, q,
                                 seq_along(query_prompts))
        names(query_prompts) <- query_ids
        all_prompt_map <- c(all_prompt_map, as.list(query_prompts))
        
        prompt_index[[sprintf("b%d_q%d", b, q)]] <- list(
          type       = "query",
          brand_idx  = b,
          brand_id   = bid,
          brand_name = brand_name,
          query_idx  = q,
          query_id   = qid,
          ids        = query_ids
        )
      }
    }
  }
  
  total_prompts <- length(all_prompt_map)
  message(sprintf("  Submitting single batch: %d prompts for %d brands, %d queries",
                  total_prompts, nrow(brands), nrow(queries)))
  
  # ── Phase 2: Submit ONE batch ──────────────────────────────────
  all_responses <- tryCatch(
    run_full_batch(
      all_prompt_map,
      model         = model,
      temperature   = 0.3,
      poll_interval = 30,
      max_wait_mins = 120
    ),
    error = function(e) {
      message(sprintf("  Batch failed: %s — falling back to async", e$message))
      resps <- ask_chatgpt_async(
        unlist(all_prompt_map),
        model       = model,
        temperature = 0.3
      )
      setNames(as.list(resps), names(all_prompt_map))
    }
  )
  
  # ── Phase 3: Score each brand and query from responses ─────────
  for (key in names(prompt_index)) {
    entry <- prompt_index[[key]]
    
    b          <- entry$brand_idx
    bid        <- entry$brand_id
    brand_name <- entry$brand_name
    
    # Extract responses for this entry
    resps <- lapply(entry$ids, function(id) all_responses[[id]])
    
    tryCatch({
      
      if (entry$type == "brand") {
        
        # 80 responses: first 50 = presence (5 × 10), last 30 = perception (3 × 10)
        presence_responses <- data.frame(
          prompt    = rep("profile_presence", reps * 5),
          responses = sanitise_text(unlist(resps[1:50])),
          stringsAsFactors = FALSE
        )
        
        perception_text <- sanitise_text(unlist(resps[51:80]))
        
        presence_score   <- presence_prompt_calc(brand_name, presence_responses)
        sentiment_result <- analyze_sentiment_weighted(perception_text)
        perception_score <- sentiment_result$score
        
        prestige_score <- calculate_prestige_from_prompts_sep(
          brand_name    = brand_name,
          brand_id      = bid,
          login_id      = login_id,
          rel_responses = presence_responses,
          db_con        = con
        )
        
        presence_history <- dbGetQuery(con,
                                       "SELECT date, presence_score FROM fact_profile_brand_history
           WHERE profile_id = $1 AND brand_id = $2
           ORDER BY date",
                                       params = list(profile_id, bid))
        
        persistence_score <- if (nrow(presence_history) < 5) {
          presence_score
        } else {
          calculate_daily_persistence_sep(presence_history)
        }
        
        airr_score <- presence_score    * AIRR_WEIGHTS$presence    +
          perception_score  * AIRR_WEIGHTS$perception  +
          prestige_score    * AIRR_WEIGHTS$prestige     +
          persistence_score * AIRR_WEIGHTS$persistence
        
        dbExecute(con, "
          INSERT INTO fact_profile_brand_history
            (profile_id, brand_id, date, presence_score, perception_score,
             prestige_score, persistence_score, airr_score)
          VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
          ON CONFLICT (profile_id, brand_id, date) DO UPDATE SET
            presence_score    = EXCLUDED.presence_score,
            perception_score  = EXCLUDED.perception_score,
            prestige_score    = EXCLUDED.prestige_score,
            persistence_score = EXCLUDED.persistence_score,
            airr_score        = EXCLUDED.airr_score",
                  params = list(profile_id, bid, Sys.Date(),
                                presence_score, perception_score,
                                prestige_score, persistence_score, airr_score))
        
        message(sprintf("    \u2713 Brand %s scored for profile %d",
                        brand_name, profile_id))
        
      } else {
        # Query scoring — 10 responses
        q      <- entry$query_idx
        qid    <- entry$query_id
        
        rel_responses <- data.frame(
          prompt    = rep("profile_query", reps),
          responses = sanitise_text(unlist(resps)),
          stringsAsFactors = FALSE
        )
        
        presence_score <- presence_prompt_calc(brand_name, rel_responses)
        
        prestige_score <- calculate_prestige_from_prompts_sep(
          brand_name    = brand_name,
          brand_id      = bid,
          login_id      = login_id,
          rel_responses = rel_responses,
          db_con        = con
        )
        
        perception_score <- calculate_perception_from_prompts_sep(
          brand_name, bid, rel_responses)
        
        presence_history <- dbGetQuery(con,
                                       "SELECT date, presence_score FROM fact_profile_query_history
           WHERE profile_id = $1 AND brand_id = $2 AND query_id = $3
           ORDER BY date",
                                       params = list(profile_id, bid, qid))
        
        persistence_score <- if (nrow(presence_history) < 5) {
          presence_score
        } else {
          calculate_daily_persistence_sep(presence_history)
        }
        
        airr_score <- presence_score    * AIRR_WEIGHTS$presence    +
          perception_score  * AIRR_WEIGHTS$perception  +
          prestige_score    * AIRR_WEIGHTS$prestige     +
          persistence_score * AIRR_WEIGHTS$persistence
        
        dbExecute(con, "
          INSERT INTO fact_profile_query_history
            (profile_id, brand_id, query_id, date, presence_score,
             perception_score, prestige_score, persistence_score, airr_score)
          VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)
          ON CONFLICT (profile_id, brand_id, query_id, date) DO UPDATE SET
            presence_score    = EXCLUDED.presence_score,
            perception_score  = EXCLUDED.perception_score,
            prestige_score    = EXCLUDED.prestige_score,
            persistence_score = EXCLUDED.persistence_score,
            airr_score        = EXCLUDED.airr_score",
                  params = list(profile_id, bid, qid, Sys.Date(),
                                presence_score, perception_score,
                                prestige_score, persistence_score, airr_score))
        
        message(sprintf("    \u2713 Prompt %d scored for brand %s, profile %d",
                        qid, brand_name, profile_id))
      }
      
    }, error = function(e) {
      message(sprintf("    \u2717 Failed %s (%s): %s",
                      entry$type, brand_name, e$message))
    })
  }
}

# ============================================================
# score_profile_sequential — original logic, used for interactive jobs
# ============================================================

score_profile_sequential <- function(con, login_id, profile_id, prefix,
                                     brands, queries, model) {
  
  for (b in seq_len(nrow(brands))) {
    bid        <- brands$brand_id[b]
    brand_name <- brands$brand_name[b]
    reach      <- brands$brand_reach[b] %||% "global"
    country    <- if (is.na(brands$reach_country[b]))  NULL else brands$reach_country[b]
    region     <- if (is.na(brands$reach_region[b]))   NULL else brands$reach_region[b]
    postcode   <- if (is.na(brands$reach_postcode[b])) NULL else brands$reach_postcode[b]
    
    message(sprintf("  Scoring brand: %s (ID: %d)", brand_name, bid))
    
    tryCatch(
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
        model          = model,
        use_batch      = FALSE
      ),
      error = function(e) message(sprintf("  \u2717 Brand %s: %s",
                                          brand_name, e$message))
    )
    
    if (nrow(queries) > 0) {
      for (q in seq_len(nrow(queries))) {
        tryCatch(
          score_profile_prompt(
            con            = con,
            brand_id       = bid,
            brand_name     = brand_name,
            profile_id     = profile_id,
            login_id       = login_id,
            query_id       = queries$query_id[q],
            query_string   = queries$query_string[q],
            prefix         = prefix,
            brand_reach    = reach,
            reach_country  = country,
            reach_region   = region,
            reach_postcode = postcode,
            model          = model,
            use_batch      = FALSE
          ),
          error = function(e) message(sprintf("  \u2717 Query %d / %s: %s",
                                              queries$query_id[q],
                                              brand_name, e$message))
        )
      }
    }
  }
}

# ============================================================
# score_profile_brand + score_profile_prompt
# Kept for interactive/sequential use — unchanged from original
# ============================================================

score_profile_brand <- function(con, brand_id, brand_name, profile_id,
                                login_id, prefix,
                                brand_reach    = "global",
                                reach_country  = NULL,
                                reach_region   = NULL,
                                reach_postcode = NULL,
                                model          = "gpt-4o-mini",
                                use_batch      = FALSE) {
  
  reach_context <- build_reach_context(brand_reach, reach_country,
                                       reach_region, reach_postcode)
  reach_str     <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
  near_me_str   <- build_near_me_suffix(brand_reach, reach_country, reach_postcode)
  
  industry <- tryCatch({
    result <- dbGetQuery(con,
                         "SELECT industry FROM fact_user_brands_tracked
       WHERE login_id = $1 AND brand_id = $2
         AND date_valid_from <= CURRENT_DATE
         AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
       LIMIT 1",
                         params = list(login_id, brand_id))
    if (nrow(result) > 0 && !is.na(result$industry[1])) result$industry[1] else NULL
  }, error = function(e) NULL)
  
  industry_str <- if (!is.null(industry) && nzchar(industry)) industry else "their industry"
  loc_str      <- if (nzchar(near_me_str)) near_me_str else reach_str
  reps         <- 10
  
  all_prompts <- c(
    rep(paste0(prefix, 'What are the best brands in the ', industry_str,
               ' industry', loc_str, '?'), reps),
    rep(paste0(prefix, 'What are the top brands in ', industry_str,
               loc_str, '?'), reps),
    rep(paste0(prefix, 'Which companies lead the ', industry_str,
               ' industry', loc_str, '?'), reps),
    rep(paste0(prefix, 'Recommend some ', industry_str, ' brands', loc_str), reps),
    rep(paste0(prefix, 'What are popular ', industry_str,
               ' companies', loc_str, '?'), reps),
    rep(paste0(prefix, 'Tell me about ', brand_name, reach_str), reps),
    rep(paste0(prefix, 'Is ', brand_name,
               ' a good choice for someone like me', reach_str, '?'), reps),
    rep(paste0(prefix, 'What do people like me think of ',
               brand_name, reach_str, '?'), reps)
  )
  
  responses <- if (use_batch) {
    ask_chatgpt_batch(all_prompts, model = model,
                      temperature = 0.3, poll_interval = 30, max_wait_mins = 60)
  } else {
    ask_chatgpt_async(all_prompts, model = model, temperature = 0.3)
  }
  
  idx  <- 0
  grab <- function(n) { r <- responses[(idx+1):(idx+n)]; idx <<- idx+n; r }
  
  pres1 <- grab(reps); pres2 <- grab(reps); pres3 <- grab(reps)
  pres4 <- grab(reps); pres5 <- grab(reps)
  perc1 <- grab(reps); perc2 <- grab(reps); perc3 <- grab(reps)
  
  all_presence_responses <- data.frame(
    prompt    = rep("profile_presence", reps * 5),
    responses = sanitise_text(unlist(c(pres1, pres2, pres3, pres4, pres5))),
    stringsAsFactors = FALSE
  )
  
  presence_score   <- presence_prompt_calc(brand_name, all_presence_responses)
  sentiment_result <- analyze_sentiment_weighted(
    sanitise_text(unlist(c(perc1, perc2, perc3))))
  perception_score <- sentiment_result$score
  
  prestige_score <- calculate_prestige_from_prompts_sep(
    brand_name = brand_name, brand_id = brand_id, login_id = login_id,
    rel_responses = all_presence_responses, db_con = con)
  
  presence_history <- dbGetQuery(con,
                                 "SELECT date, presence_score FROM fact_profile_brand_history
     WHERE profile_id = $1 AND brand_id = $2 ORDER BY date",
                                 params = list(profile_id, brand_id))
  
  persistence_score <- if (nrow(presence_history) < 5) presence_score else
    calculate_daily_persistence_sep(presence_history)
  
  airr_score <- presence_score    * AIRR_WEIGHTS$presence    +
    perception_score  * AIRR_WEIGHTS$perception  +
    prestige_score    * AIRR_WEIGHTS$prestige     +
    persistence_score * AIRR_WEIGHTS$persistence
  
  dbExecute(con, "
    INSERT INTO fact_profile_brand_history
      (profile_id, brand_id, date, presence_score, perception_score,
       prestige_score, persistence_score, airr_score)
    VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
    ON CONFLICT (profile_id, brand_id, date) DO UPDATE SET
      presence_score    = EXCLUDED.presence_score,
      perception_score  = EXCLUDED.perception_score,
      prestige_score    = EXCLUDED.prestige_score,
      persistence_score = EXCLUDED.persistence_score,
      airr_score        = EXCLUDED.airr_score",
            params = list(profile_id, brand_id, Sys.Date(),
                          presence_score, perception_score,
                          prestige_score, persistence_score, airr_score))
  
  message(sprintf("    \u2713 Brand %s scored for profile %d",
                  brand_name, profile_id))
}

score_profile_prompt <- function(con, brand_id, brand_name, profile_id,
                                 login_id, query_id, query_string, prefix,
                                 brand_reach    = "global",
                                 reach_country  = NULL,
                                 reach_region   = NULL,
                                 reach_postcode = NULL,
                                 model          = "gpt-4o-mini",
                                 use_batch      = FALSE) {
  
  query_resolved  <- inject_near_me_location(query_string, brand_reach,
                                             reach_country, reach_region,
                                             reach_postcode)
  profiled_prompt <- paste0(prefix, query_resolved)
  
  rel_responses <- prompt_queries(rep(profiled_prompt, 10),
                                  model = model, use_batch = use_batch)
  
  presence_score   <- presence_prompt_calc(brand_name, rel_responses)
  prestige_score   <- calculate_prestige_from_prompts_sep(
    brand_name = brand_name, brand_id = brand_id, login_id = login_id,
    rel_responses = rel_responses, db_con = con)
  perception_score <- calculate_perception_from_prompts_sep(
    brand_name, brand_id, rel_responses)
  
  presence_history <- dbGetQuery(con,
                                 "SELECT date, presence_score FROM fact_profile_query_history
     WHERE profile_id = $1 AND brand_id = $2 AND query_id = $3
     ORDER BY date",
                                 params = list(profile_id, brand_id, query_id))
  
  persistence_score <- if (nrow(presence_history) < 5) presence_score else
    calculate_daily_persistence_sep(presence_history)
  
  airr_score <- presence_score    * AIRR_WEIGHTS$presence    +
    perception_score  * AIRR_WEIGHTS$perception  +
    prestige_score    * AIRR_WEIGHTS$prestige     +
    persistence_score * AIRR_WEIGHTS$persistence
  
  dbExecute(con, "
    INSERT INTO fact_profile_query_history
      (profile_id, brand_id, query_id, date, presence_score, perception_score,
       prestige_score, persistence_score, airr_score)
    VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)
    ON CONFLICT (profile_id, brand_id, query_id, date) DO UPDATE SET
      presence_score    = EXCLUDED.presence_score,
      perception_score  = EXCLUDED.perception_score,
      prestige_score    = EXCLUDED.prestige_score,
      persistence_score = EXCLUDED.persistence_score,
      airr_score        = EXCLUDED.airr_score",
            params = list(profile_id, brand_id, query_id, Sys.Date(),
                          presence_score, perception_score,
                          prestige_score, persistence_score, airr_score))
  
  message(sprintf("    \u2713 Prompt %d scored for brand %s, profile %d",
                  query_id, brand_name, profile_id))
}