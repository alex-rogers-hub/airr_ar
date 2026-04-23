# /global_scripts/worker_orchestrator.R
# Filtered versions of daily_refresh_loop_batch and daily_prompt_loop
# for use inside AWS Batch containers.
# 
# Each function is identical to its counterpart in upload_functions.R
# except: 
#   1. login_ids is passed in rather than fetched from DB
#   2. The initial DB query filters to those login_ids only
#   3. make_con() is NOT called — con is passed in or created once

# ============================================================
# Helper: build SQL IN clause safely from a vector of integers
# ============================================================

make_in_clause <- function(login_ids) {
  # Ensure integers only — guard against injection
  ids <- as.integer(login_ids)
  if (any(is.na(ids))) stop("login_ids contained non-integer values")
  paste(ids, collapse = ", ")
}

# ============================================================
# FILTERED BRAND + PERSONA LOOP
# Mirrors daily_refresh_loop_batch() exactly.
# Only change: user_brand_list query filtered to login_ids.
# ============================================================

daily_refresh_loop_batch_for <- function(con, login_ids,
                                         model = "gpt-4.1-mini") {
  
  ids_sql <- make_in_clause(login_ids)
  
  user_brand_list <- dbGetQuery(con, sprintf("
    SELECT ubt.login_id, b.brand_id, b.brand_name,
           b.brand_reach, b.reach_country, b.reach_region,
           b.reach_postcode, ubt.industry
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
      AND ubt.login_id IN (%s)
    ORDER BY ubt.login_id, b.brand_name",
                                             ids_sql
  ))
  
  n_brands <- nrow(user_brand_list)
  message(sprintf(
    "=== [Refresh] Group has %d brand-account pairs across %d accounts ===",
    n_brands, length(login_ids)
  ))
  
  if (n_brands == 0) {
    message("No brands to score in this group.")
    return(invisible(c()))
  }
  
  # ── PHASE 1: Build ALL prompts for ALL brands ───────────────────
  # Identical to daily_refresh_loop_batch — no changes
  message("\n--- Phase 1: Building prompt list ---")
  
  all_prompt_map   <- list()
  brand_prompt_map <- list()
  
  for (i in seq_len(n_brands)) {
    row      <- user_brand_list[i, ]
    brand    <- row$brand_name
    lid      <- row$login_id
    industry <- if (is.na(row$industry))      NULL else row$industry
    reach    <- row$brand_reach %||% "global"
    country  <- if (is.na(row$reach_country)) NULL else row$reach_country
    region   <- if (is.na(row$reach_region))  NULL else row$reach_region
    postcode <- if (is.na(row$reach_postcode))NULL else row$reach_postcode
    
    reach_context <- build_reach_context(reach, country, region, postcode)
    reach_str     <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
    near_me_str   <- build_near_me_suffix(reach, country, postcode)
    has_industry  <- !is.null(industry) && nzchar(industry)
    industry_str  <- if (has_industry) industry else "their industry"
    
    reps   <- 10
    prefix <- sprintf("brand%d_user%d", i, lid)
    
    # ── Prompt list — copied verbatim from daily_refresh_loop_batch ──
    prompts_this_brand <- c(
      rep(paste0('Who are the top 10 competitors for the brand ', brand,
                 reach_str,
                 '? Return only the brand names separated by semi-colons and no other information'), reps),
      rep(paste0('In what industry is the brand ', brand, reach_str,
                 '? Give your answer in the form "brand is in the __ industry", giving no other information'), reps),
      rep(paste0('Tell me about the brand ', brand, reach_str), reps),
      rep(paste0("When was ", brand,
                 " founded? Give only the year, no other words."), reps),
      rep(paste0("Where is ", brand, " headquartered", reach_str,
                 "? Give only the city and country."), reps),
      rep(paste0("Who is the current CEO of ", brand,
                 "? Give only the name."), reps),
      rep(paste0("What industry is ", brand, " in", reach_str,
                 "? Give a one-sentence answer."), reps),
      rep(paste0("Is ", brand,
                 " owned by a parent company? If so, which one? Give only the name of the company or the word 'none'"), reps),
      rep(paste0("What are the main products or services of ", brand, reach_str,
                 "? List the top 3."), reps),
      rep(paste0("How would you describe ", brand, "'s pricing strategy",
                 reach_str,
                 ": budget, mid-range, or premium? Only respond with one of those three options"), reps),
      rep(paste0("Who is ", brand, "'s primary target market", reach_str, "?"), reps),
      rep(paste0("What was a major milestone in ", brand, "'s history?"), reps),
      rep(paste0("How did ", brand, " get started? Give a brief summary."), reps),
      rep(paste0("Approximately how many employees does ", brand,
                 " have? Give the answer as a number only"), reps),
      rep(paste0("What is the approximate annual revenue of ", brand,
                 "? Give the answer as a number of US dollars only"), reps),
      rep(paste0("In which countries or regions does ", brand, " operate",
                 reach_str,
                 "? List only the country names with no extra information"), reps),
      rep(paste0('What are the best brands in the ', industry_str,
                 ' industry',
                 if (nzchar(near_me_str)) near_me_str else reach_str, '?'), reps),
      rep(paste0("What are the top brands in ", industry_str,
                 if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
      rep(paste0("Which companies lead the ", industry_str, " industry",
                 if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
      rep(paste0("Recommend some ", industry_str, " brands",
                 if (nzchar(near_me_str)) near_me_str else reach_str), reps),
      rep(paste0("What are popular ", industry_str, " companies",
                 if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
      rep(paste0("List major players in ", industry_str,
                 if (nzchar(near_me_str)) near_me_str else reach_str), reps)
    )
    
    ids_this_brand             <- sprintf("%s_p%03d", prefix,
                                          seq_along(prompts_this_brand))
    names(prompts_this_brand)  <- ids_this_brand
    all_prompt_map             <- c(all_prompt_map,
                                    as.list(prompts_this_brand))
    brand_prompt_map[[prefix]] <- ids_this_brand
    
    message(sprintf("  Built %d prompts for brand %d/%d: %s",
                    length(prompts_this_brand), i, n_brands, brand))
  }
  
  message(sprintf(
    "\n--- Phase 1 complete: %d total prompts for %d brands ---",
    length(all_prompt_map), n_brands
  ))
  
  # ── PHASE 2: Submit ONE batch for this group ────────────────────
  message("\n--- Phase 2: Submitting batch ---")
  
  all_responses <- tryCatch(
    run_full_batch(
      all_prompt_map,
      model         = model,
      temperature   = 0.1,
      poll_interval = 60,
      max_wait_mins = 180
    ),
    error = function(e) {
      message(sprintf(
        "Batch failed: %s\nFalling back to async...", e$message))
      resps <- ask_chatgpt_async(
        unlist(all_prompt_map),
        model       = model,
        temperature = 0.1
      )
      setNames(resps, names(all_prompt_map))
    }
  )
  
  # ── PHASE 3: Score each brand ───────────────────────────────────
  # Identical to daily_refresh_loop_batch Phase 3 — no changes
  message("\n--- Phase 3: Scoring brands ---")
  
  failed <- c()
  
  for (i in seq_len(n_brands)) {
    row    <- user_brand_list[i, ]
    brand  <- row$brand_name
    lid    <- row$login_id
    bid    <- row$brand_id
    prefix <- sprintf("brand%d_user%d", i, lid)
    
    tryCatch({
      t0   <- Sys.time()
      ids  <- brand_prompt_map[[prefix]]
      resps <- lapply(ids, function(id) all_responses[[id]])
      
      idx  <- 0
      grab <- function(n) {
        result <- resps[(idx + 1):(idx + n)]
        idx   <<- idx + n
        unlist(result)
      }
      
      competitor_raw   <- grab(10); industry_raw     <- grab(10)
      description_raw  <- grab(10); founded_raw      <- grab(10)
      hq_raw           <- grab(10); ceo_raw          <- grab(10)
      industry_det_raw <- grab(10); parent_raw       <- grab(10)
      products_raw     <- grab(10); pricing_raw      <- grab(10)
      target_raw       <- grab(10); milestone_raw    <- grab(10)
      origin_raw       <- grab(10); employees_raw    <- grab(10)
      revenue_raw      <- grab(10); countries_raw    <- grab(10)
      best_brands_raw  <- grab(10); top_brands_raw   <- grab(10)
      leading_raw      <- grab(10); rec_brands_raw   <- grab(10)
      popular_raw      <- grab(10); major_raw        <- grab(10)
      
      get_data <- list(
        competitor_summary = competitor_raw %>%
          unlist() %>% str_split(";") %>% unlist() %>% str_trim() %>%
          tibble(competitor = .) %>%
          filter(nchar(competitor) > 0) %>%
          count(competitor, sort = TRUE, name = "mentions") %>%
          head(9),
        industry_summary = industry_raw %>%
          unlist() %>% tibble(industry = .) %>%
          mutate(industry_desc = substr(industry,
                                        12 + nchar(brand),
                                        nchar(industry) - 10)) %>%
          filter(nchar(industry_desc) > 0) %>%
          count(industry_desc, sort = TRUE, name = "mentions") %>%
          head(1),
        comp_second_response_list = as.list(best_brands_raw),
        description_list          = as.list(description_raw),
        presence_data = list(
          top_brands        = list(data = as.list(top_brands_raw),
                                   type = "text",
                                   question = "Top brands"),
          leading_brands    = list(data = as.list(leading_raw),
                                   type = "text",
                                   question = "Leading brands"),
          rec_brands        = list(data = as.list(rec_brands_raw),
                                   type = "text",
                                   question = "Recommended brands"),
          popular_companies = list(data = as.list(popular_raw),
                                   type = "text",
                                   question = "Popular companies"),
          major_players     = list(data = as.list(major_raw),
                                   type = "text",
                                   question = "Major players")
        ),
        perception_data = list(
          founded           = list(data = as.list(founded_raw),
                                   type = "year",        question = "Founded"),
          hq                = list(data = as.list(hq_raw),
                                   type = "location",    question = "HQ"),
          ceo               = list(data = as.list(ceo_raw),
                                   type = "name",        question = "CEO"),
          industry          = list(data = as.list(industry_det_raw),
                                   type = "text",        question = "Industry"),
          parent_company    = list(data = as.list(parent_raw),
                                   type = "categorical", question = "Parent"),
          products_services = list(data = as.list(products_raw),
                                   type = "list",        question = "Products"),
          pricing           = list(data = as.list(pricing_raw),
                                   type = "categorical", question = "Pricing"),
          target_market     = list(data = as.list(target_raw),
                                   type = "text",        question = "Target"),
          milestone         = list(data = as.list(milestone_raw),
                                   type = "text",        question = "Milestone"),
          start             = list(data = as.list(origin_raw),
                                   type = "text",        question = "Origin"),
          num_employees     = list(data = as.list(employees_raw),
                                   type = "numeric",     question = "Employees"),
          revenue           = list(data = as.list(revenue_raw),
                                   type = "numeric",     question = "Revenue"),
          countries         = list(data = as.list(countries_raw),
                                   type = "list",        question = "Countries")
        )
      )
      
      search_names <- get_brand_search_names(bid, brand, con)
      presence     <- calculate_presence_from_prompts(search_names,
                                                      get_data$presence_data)
      perception   <- calculate_perception_from_prompts(brand, get_data)
      prestige     <- calculate_prestige_from_prompts(brand, get_data)
      
      airr_scores  <- list(presence   = presence,
                           perception = perception,
                           prestige   = prestige)
      
      upload_daily_refresh(con, brand, lid, airr_scores, Sys.Date())
      calc_daily_persistance(con, brand, lid, Sys.Date())
      calc_daily_airr(con, brand, lid, Sys.Date())
      
      message(sprintf("  \u2713 [%d/%d] %s / user %d (%.0fs)",
                      i, n_brands, brand, lid,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      
    }, error = function(e) {
      failed <<- c(failed, paste0(brand, "/user:", lid))
      message(sprintf("  \u2717 [%d/%d] %s: %s",
                      i, n_brands, brand, e$message))
    })
  }
  
  # ── PHASE 4: Personas ───────────────────────────────────────────
  # Mirrors the persona section of daily_refresh_loop — filtered to login_ids
  message("\n--- Phase 4: Scoring customer profiles ---")
  
  active_profiles <- dbGetQuery(con, sprintf("
    SELECT DISTINCT upt.login_id, upt.profile_id, dcp.profile_name
    FROM fact_user_profiles_tracked upt
    JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
    JOIN fact_user_sub_level fus  ON fus.login_id   = upt.login_id
    JOIN dim_subscription ds      ON ds.subscription_level_id
                                   = fus.subscription_level_id
    WHERE upt.date_valid_from <= CURRENT_DATE
      AND (upt.date_valid_to IS NULL OR upt.date_valid_to > CURRENT_DATE)
      AND fus.date_valid_from <= CURRENT_DATE
      AND (fus.date_valid_to IS NULL OR fus.date_valid_to > CURRENT_DATE)
      AND upt.login_id IN (%s)
    ORDER BY upt.login_id, upt.profile_id",
                                             ids_sql
  ))
  
  if (nrow(active_profiles) == 0) {
    message("No active profiles in this group.")
  } else {
    message(sprintf("  %d profiles to score", nrow(active_profiles)))
    profile_failed <- c()
    
    for (p in seq_len(nrow(active_profiles))) {
      lid   <- active_profiles$login_id[p]
      pid   <- active_profiles$profile_id[p]
      pname <- active_profiles$profile_name[p]
      
      tryCatch({
        t0 <- Sys.time()
        message(sprintf("  [%d/%d] %s (login: %d)...",
                        p, nrow(active_profiles), pname, lid))
        score_profile(con, lid, pid, model, use_batch = TRUE)
        message(sprintf("  \u2713 Done in %.1fs",
                        as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      }, error = function(e) {
        profile_failed <<- c(profile_failed, pname)
        message(sprintf("  \u2717 %s: %s", pname, e$message))
      })
    }
    
    if (length(profile_failed) > 0) {
      message(sprintf("Profile failures: %s",
                      paste(profile_failed, collapse = ", ")))
    }
  }
  
  # ── Summary ─────────────────────────────────────────────────────
  if (length(failed) > 0) {
    message(sprintf("\n%d brand failures: %s",
                    length(failed), paste(failed, collapse = ", ")))
  } else {
    message(sprintf("\n\u2713 All %d brands scored", n_brands))
  }
  
  return(invisible(failed))
}

# ============================================================
# FILTERED PROMPT LOOP
# Mirrors daily_prompt_loop() exactly.
# Only change: queries filtered to login_ids.
# ============================================================

daily_prompt_loop_for <- function(con, login_ids,
                                  model = "gpt-4.1-mini") {
  
  ids_sql <- make_in_clause(login_ids)
  
  # Only fetch queries tracked by accounts in this group
  # (A query shared across groups gets scored once per group —
  # that's fine; ON CONFLICT DO UPDATE means last writer wins,
  # which is identical to the current single-process behaviour)
  active_queries <- dbGetQuery(con, sprintf("
    SELECT DISTINCT dq.query_id, dq.query_string
    FROM dim_query dq
    INNER JOIN fact_user_queries_tracked uqt ON dq.query_id = uqt.query_id
    WHERE uqt.date_valid_from <= CURRENT_DATE
      AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
      AND uqt.login_id IN (%s)
    ORDER BY dq.query_id",
                                            ids_sql
  ))
  
  if (nrow(active_queries) == 0) {
    message("No active queries in this group.")
    return(invisible(c()))
  }
  
  message(sprintf(
    "=== [Prompts] Group has %d queries across %d accounts ===",
    nrow(active_queries), length(login_ids)
  ))
  
  failed <- c()
  
  for (q in seq_len(nrow(active_queries))) {
    qid     <- active_queries$query_id[q]
    qstring <- active_queries$query_string[q]
    
    tryCatch({
      message(sprintf("\n[%d/%d] Query: %s (ID: %d)...",
                      q, nrow(active_queries),
                      substr(qstring, 1, 50), qid))
      
      # Only brands belonging to accounts in this group
      brands_for_query <- dbGetQuery(con, sprintf("
        SELECT DISTINCT b.brand_id, b.brand_name,
               b.brand_reach, b.reach_country, b.reach_region,
               b.reach_postcode, ubt.login_id
        FROM fact_user_queries_tracked uqt
        JOIN fact_user_brands_tracked ubt
          ON uqt.login_id = ubt.login_id
        JOIN dim_brand b  ON b.brand_id  = ubt.brand_id
        JOIN dim_brand_query dbq
          ON dbq.brand_id = b.brand_id AND dbq.query_id = uqt.query_id
        WHERE uqt.query_id = $1
          AND uqt.date_valid_from <= CURRENT_DATE
          AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
          AND ubt.date_valid_from <= CURRENT_DATE
          AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
          AND uqt.login_id IN (%s)
        ORDER BY b.brand_id, ubt.login_id",
                                                  ids_sql),
                                     params = list(qid)
      )
      
      if (nrow(brands_for_query) == 0) {
        message("  No active brands in this group for this query, skipping.")
        next
      }
      
      message(sprintf("  %d brand/user combinations to score",
                      nrow(brands_for_query)))
      
      # ── Everything below is verbatim from daily_prompt_loop ──────
      
      brands_for_query$query_resolved <- mapply(
        function(reach, country, region, postcode) {
          country  <- if (is.na(country))  NULL else country
          region   <- if (is.na(region))   NULL else region
          postcode <- if (is.na(postcode)) NULL else postcode
          inject_near_me_location(qstring, reach, country, region, postcode)
        },
        brands_for_query$brand_reach,
        brands_for_query$reach_country,
        brands_for_query$reach_region,
        brands_for_query$reach_postcode
      )
      
      unique_prompts  <- unique(brands_for_query$query_resolved)
      responses_cache <- list()
      
      for (uq in unique_prompts) {
        message(sprintf("  Fetching responses for: %s", substr(uq, 1, 60)))
        responses_cache[[uq]] <- prompt_queries(
          rep(uq, 10),
          model     = model,
          use_batch = TRUE
        )
      }
      
      for (bid in unique(brands_for_query$brand_id)) {
        dbExecute(con,
                  "INSERT INTO dim_brand_query (brand_id, query_id, date_added)
           VALUES ($1,$2,$3) ON CONFLICT (brand_id, query_id) DO NOTHING",
                  params = list(bid, qid, Sys.Date()))
      }
      
      for (b in seq_len(nrow(brands_for_query))) {
        bid        <- brands_for_query$brand_id[b]
        brand_name <- brands_for_query$brand_name[b]
        lid        <- brands_for_query$login_id[b]
        rel_resp   <- responses_cache[[brands_for_query$query_resolved[b]]]
        
        search_names <- get_brand_search_names(bid, brand_name, con)
        
        tryCatch({
          presence_score <- presence_prompt_calc(search_names, rel_resp)
          
          prestige_score <- calculate_prestige_from_prompts_sep(
            brand_name    = brand_name,
            brand_id      = bid,
            login_id      = lid,
            rel_responses = rel_resp,
            db_con        = con
          )
          
          perception_score <- calculate_perception_from_prompts_sep(
            brand_name, bid, rel_resp)
          
          dbExecute(con, "
            INSERT INTO fact_query_history (
              brand_id, query_id, date, presence_score, perception_score,
              prestige_score, persistence_score, airr_score
            ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
            ON CONFLICT (brand_id, query_id, date) DO UPDATE SET
              presence_score    = EXCLUDED.presence_score,
              perception_score  = EXCLUDED.perception_score,
              prestige_score    = EXCLUDED.prestige_score,
              persistence_score = EXCLUDED.persistence_score,
              airr_score        = EXCLUDED.airr_score",
                    params = list(bid, qid, Sys.Date(),
                                  presence_score, perception_score,
                                  prestige_score, 0, 0))
          
          ph <- dbGetQuery(con,
                           "SELECT date, presence_score FROM fact_query_history
             WHERE brand_id = $1 AND date <= $2 AND query_id = $3
             ORDER BY date",
                           params = list(bid, Sys.Date(), qid))
          
          persistence_score <- if (nrow(ph) < 5) presence_score else
            calculate_daily_persistence_sep(ph)
          
          ts <- dbGetQuery(con,
                           "SELECT presence_score, perception_score, prestige_score
             FROM fact_query_history
             WHERE brand_id = $1 AND date = $2 AND query_id = $3",
                           params = list(bid, Sys.Date(), qid))
          
          airr <- ts$perception_score * AIRR_WEIGHTS$perception +
            ts$presence_score   * AIRR_WEIGHTS$presence   +
            ts$prestige_score   * AIRR_WEIGHTS$prestige   +
            persistence_score   * AIRR_WEIGHTS$persistence
          
          dbExecute(con,
                    "UPDATE fact_query_history
             SET persistence_score = $1, airr_score = $2
             WHERE brand_id = $3 AND date = $4 AND query_id = $5",
                    params = list(persistence_score, airr,
                                  bid, Sys.Date(), qid))
          
          message(sprintf("    \u2713 %s (user: %d)", brand_name, lid))
          
        }, error = function(e) {
          failed <<- c(failed,
                       paste0("brand:", bid, "/user:", lid, "/query:", qid))
          message(sprintf("    \u2717 %s: %s", brand_name, e$message))
        })
      }
      
    }, error = function(e) {
      failed <<- c(failed, paste0("query:", qid))
      message(sprintf("  \u2717 Query %d failed: %s", qid, e$message))
    })
  }
  
  if (length(failed) > 0) {
    message(sprintf("\n%d failures: %s",
                    length(failed), paste(failed, collapse = ", ")))
  } else {
    message("\nAll queries completed!")
  }
  
  return(invisible(failed))
}

# ============================================================
# MAIN ENTRY POINT — called by worker_entry.R
# Runs both loops in parallel, mirrors run_daily.sh behaviour
# ============================================================

worker_score_accounts <- function(con, login_ids, model, run_id) {
  
  message(sprintf(
    "Starting scoring for %d accounts [run: %s]",
    length(login_ids), run_id
  ))
  
  ids_sql <- make_in_clause(login_ids)
  
  # ── Fetch everything we need upfront ──────────────────────────
  message("Fetching account data...")
  
  user_brand_list <- dbGetQuery(con, sprintf("
    SELECT ubt.login_id, b.brand_id, b.brand_name,
           b.brand_reach, b.reach_country, b.reach_region,
           b.reach_postcode, ubt.industry
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
      AND ubt.login_id IN (%s)
    ORDER BY ubt.login_id, b.brand_name", ids_sql))
  
  active_queries <- dbGetQuery(con, sprintf("
    SELECT DISTINCT dq.query_id, dq.query_string
    FROM dim_query dq
    JOIN fact_user_queries_tracked uqt ON dq.query_id = uqt.query_id
    WHERE uqt.date_valid_from <= CURRENT_DATE
      AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
      AND uqt.login_id IN (%s)
    ORDER BY dq.query_id", ids_sql))
  
  active_profiles <- dbGetQuery(con, sprintf("
    SELECT DISTINCT upt.login_id, upt.profile_id, dcp.profile_name,
                    dcp.profile_descriptor
    FROM fact_user_profiles_tracked upt
    JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
    JOIN fact_user_sub_level fus  ON fus.login_id   = upt.login_id
    JOIN dim_subscription ds      ON ds.subscription_level_id
                                   = fus.subscription_level_id
    WHERE upt.date_valid_from <= CURRENT_DATE
      AND (upt.date_valid_to IS NULL OR upt.date_valid_to > CURRENT_DATE)
      AND fus.date_valid_from <= CURRENT_DATE
      AND (fus.date_valid_to IS NULL OR fus.date_valid_to > CURRENT_DATE)
      AND upt.login_id IN (%s)
    ORDER BY upt.login_id, upt.profile_id", ids_sql))
  
  n_brands   <- nrow(user_brand_list)
  n_queries  <- nrow(active_queries)
  n_profiles <- nrow(active_profiles)
  
  message(sprintf("  %d brands, %d queries, %d personas to score",
                  n_brands, n_queries, n_profiles))
  
  if (n_brands == 0) {
    message("No brands to score. Exiting.")
    return(list(refresh_failed = c(), prompt_failed = c()))
  }
  
  # ── Phase 1: Build ALL prompts in one pass ─────────────────────
  message("\n--- Phase 1: Building all prompts ---")
  
  all_prompt_map   <- list()
  brand_prompt_map <- list()   # for brand scoring
  query_prompt_map <- list()   # for query scoring
  # persona prompt map built per-profile inside score_profile_batched
  # but we include their prompts here too
  
  # Brand prompts (same as daily_refresh_loop_batch_for)
  for (i in seq_len(n_brands)) {
    row      <- user_brand_list[i, ]
    brand    <- row$brand_name
    lid      <- row$login_id
    industry <- if (is.na(row$industry))       NULL else row$industry
    reach    <- row$brand_reach %||% "global"
    country  <- if (is.na(row$reach_country))  NULL else row$reach_country
    region   <- if (is.na(row$reach_region))   NULL else row$reach_region
    postcode <- if (is.na(row$reach_postcode)) NULL else row$reach_postcode
    
    reach_context <- build_reach_context(reach, country, region, postcode)
    reach_str     <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
    near_me_str   <- build_near_me_suffix(reach, country, postcode)
    industry_str  <- if (!is.null(industry) && nzchar(industry)) industry else "their industry"
    
    reps   <- 10
    prefix <- sprintf("brand%d_user%d", i, lid)
    
    prompts_this_brand <- c(
      rep(paste0('Who are the top 10 competitors for the brand ', brand,
                 reach_str, '? Return only the brand names separated by semi-colons and no other information'), reps),
      rep(paste0('In what industry is the brand ', brand, reach_str,
                 '? Give your answer in the form "brand is in the __ industry", giving no other information'), reps),
      rep(paste0('Tell me about the brand ', brand, reach_str), reps),
      rep(paste0("When was ", brand, " founded? Give only the year, no other words."), reps),
      rep(paste0("Where is ", brand, " headquartered", reach_str, "? Give only the city and country."), reps),
      rep(paste0("Who is the current CEO of ", brand, "? Give only the name."), reps),
      rep(paste0("What industry is ", brand, " in", reach_str, "? Give a one-sentence answer."), reps),
      rep(paste0("Is ", brand, " owned by a parent company? If so, which one? Give only the name of the company or the word 'none'"), reps),
      rep(paste0("What are the main products or services of ", brand, reach_str, "? List the top 3."), reps),
      rep(paste0("How would you describe ", brand, "'s pricing strategy", reach_str,
                 ": budget, mid-range, or premium? Only respond with one of those three options"), reps),
      rep(paste0("Who is ", brand, "'s primary target market", reach_str, "?"), reps),
      rep(paste0("What was a major milestone in ", brand, "'s history?"), reps),
      rep(paste0("How did ", brand, " get started? Give a brief summary."), reps),
      rep(paste0("Approximately how many employees does ", brand, " have? Give the answer as a number only"), reps),
      rep(paste0("What is the approximate annual revenue of ", brand, "? Give the answer as a number of US dollars only"), reps),
      rep(paste0("In which countries or regions does ", brand, " operate", reach_str,
                 "? List only the country names with no extra information"), reps),
      rep(paste0('What are the best brands in the ', industry_str, ' industry',
                 if (nzchar(near_me_str)) near_me_str else reach_str, '?'), reps),
      rep(paste0("What are the top brands in ", industry_str,
                 if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
      rep(paste0("Which companies lead the ", industry_str, " industry",
                 if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
      rep(paste0("Recommend some ", industry_str, " brands",
                 if (nzchar(near_me_str)) near_me_str else reach_str), reps),
      rep(paste0("What are popular ", industry_str, " companies",
                 if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
      rep(paste0("List major players in ", industry_str,
                 if (nzchar(near_me_str)) near_me_str else reach_str), reps)
    )
    
    ids_this_brand             <- sprintf("BR_%s_p%03d", prefix,
                                          seq_along(prompts_this_brand))
    names(prompts_this_brand)  <- ids_this_brand
    all_prompt_map             <- c(all_prompt_map, as.list(prompts_this_brand))
    brand_prompt_map[[prefix]] <- ids_this_brand
  }
  
  # Query prompts — one set of 10 per unique resolved query
  # (brands share responses where query resolves identically)
  query_response_map <- list()  # query_id -> resolved -> ids
  
  for (q in seq_len(n_queries)) {
    qid     <- active_queries$query_id[q]
    qstring <- active_queries$query_string[q]
    
    # Get all brand/user combos for this query
    brands_for_query <- dbGetQuery(con, sprintf("
      SELECT DISTINCT b.brand_id, b.brand_name,
             b.brand_reach, b.reach_country, b.reach_region,
             b.reach_postcode, ubt.login_id
      FROM fact_user_queries_tracked uqt
      JOIN fact_user_brands_tracked ubt ON uqt.login_id = ubt.login_id
      JOIN dim_brand b  ON b.brand_id  = ubt.brand_id
      JOIN dim_brand_query dbq
        ON dbq.brand_id = b.brand_id AND dbq.query_id = uqt.query_id
      WHERE uqt.query_id = $1
        AND uqt.date_valid_from <= CURRENT_DATE
        AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
        AND uqt.login_id IN (%s)
      ORDER BY b.brand_id", ids_sql),
                                   params = list(qid))
    
    if (nrow(brands_for_query) == 0) next
    
    # Resolve location variants and deduplicate
    brands_for_query$query_resolved <- mapply(
      function(reach, country, region, postcode) {
        country  <- if (is.na(country))  NULL else country
        region   <- if (is.na(region))   NULL else region
        postcode <- if (is.na(postcode)) NULL else postcode
        inject_near_me_location(qstring, reach, country, region, postcode)
      },
      brands_for_query$brand_reach,
      brands_for_query$reach_country,
      brands_for_query$reach_region,
      brands_for_query$reach_postcode
    )
    
    # One set of 10 prompts per unique resolved string
    unique_resolved <- unique(brands_for_query$query_resolved)
    query_response_map[[as.character(qid)]] <- list(
      brands_for_query = brands_for_query,
      resolved_ids     = list()
    )
    
    for (uq in unique_resolved) {
      safe_key <- gsub("[^a-zA-Z0-9]", "_", substr(uq, 1, 30))
      ids      <- sprintf("QR_q%d_%s_p%02d", qid, safe_key, seq_len(10))
      prompts  <- setNames(rep(uq, 10), ids)
      all_prompt_map <- c(all_prompt_map, as.list(prompts))
      query_response_map[[as.character(qid)]]$resolved_ids[[uq]] <- ids
    }
  }
  
  # Persona prompts — collect per profile
  persona_prompt_map <- list()  # profile_id -> brand_id -> list of ids
  
  for (p in seq_len(n_profiles)) {
    lid        <- active_profiles$login_id[p]
    pid        <- active_profiles$profile_id[p]
    descriptor <- active_profiles$profile_descriptor[p]
    prefix_p   <- paste0(descriptor, " ")
    
    persona_brands <- dbGetQuery(con, "
      SELECT b.brand_id, b.brand_name, b.brand_reach,
             b.reach_country, b.reach_region, b.reach_postcode
      FROM fact_user_brands_tracked ubt
      JOIN dim_brand b ON b.brand_id = ubt.brand_id
      WHERE ubt.login_id = $1
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)",
                                 params = list(lid))
    
    persona_queries <- dbGetQuery(con, "
      SELECT dq.query_id, dq.query_string
      FROM fact_user_queries_tracked uqt
      JOIN dim_query dq ON dq.query_id = uqt.query_id
      WHERE uqt.login_id = $1
        AND uqt.date_valid_from <= CURRENT_DATE
        AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to > CURRENT_DATE)",
                                  params = list(lid))
    
    persona_prompt_map[[as.character(pid)]] <- list(
      login_id = lid,
      brands   = persona_brands,
      queries  = persona_queries,
      prefix   = prefix_p,
      brand_ids_map = list()
    )
    
    for (b in seq_len(nrow(persona_brands))) {
      bid        <- persona_brands$brand_id[b]
      brand_name <- persona_brands$brand_name[b]
      reach      <- persona_brands$brand_reach[b] %||% "global"
      country    <- if (is.na(persona_brands$reach_country[b]))  NULL else persona_brands$reach_country[b]
      region     <- if (is.na(persona_brands$reach_region[b]))   NULL else persona_brands$reach_region[b]
      postcode   <- if (is.na(persona_brands$reach_postcode[b])) NULL else persona_brands$reach_postcode[b]
      
      reach_context <- build_reach_context(reach, country, region, postcode)
      reach_str     <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
      near_me_str   <- build_near_me_suffix(reach, country, postcode)
      loc_str       <- if (nzchar(near_me_str)) near_me_str else reach_str
      
      industry <- tryCatch({
        r <- dbGetQuery(con,
                        "SELECT industry FROM fact_user_brands_tracked
           WHERE login_id=$1 AND brand_id=$2
             AND date_valid_from <= CURRENT_DATE
             AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
           LIMIT 1",
                        params = list(lid, bid))
        if (nrow(r) > 0 && !is.na(r$industry[1])) r$industry[1] else NULL
      }, error = function(e) NULL)
      industry_str <- if (!is.null(industry) && nzchar(industry)) industry else "their industry"
      
      reps <- 10
      brand_prompts <- c(
        rep(paste0(prefix_p, 'What are the best brands in the ', industry_str, ' industry', loc_str, '?'), reps),
        rep(paste0(prefix_p, 'What are the top brands in ', industry_str, loc_str, '?'), reps),
        rep(paste0(prefix_p, 'Which companies lead the ', industry_str, ' industry', loc_str, '?'), reps),
        rep(paste0(prefix_p, 'Recommend some ', industry_str, ' brands', loc_str), reps),
        rep(paste0(prefix_p, 'What are popular ', industry_str, ' companies', loc_str, '?'), reps),
        rep(paste0(prefix_p, 'Tell me about ', brand_name, reach_str), reps),
        rep(paste0(prefix_p, 'Is ', brand_name, ' a good choice for someone like me', reach_str, '?'), reps),
        rep(paste0(prefix_p, 'What do people like me think of ', brand_name, reach_str, '?'), reps)
      )
      
      bp_ids <- sprintf("PE_p%d_b%d_brand_p%03d", pid, bid,
                        seq_along(brand_prompts))
      names(brand_prompts) <- bp_ids
      all_prompt_map <- c(all_prompt_map, as.list(brand_prompts))
      persona_prompt_map[[as.character(pid)]]$brand_ids_map[[as.character(bid)]] <- list(
        brand_name = brand_name,
        brand_ids  = bp_ids,
        query_ids  = list()
      )
      
      for (q in seq_len(nrow(persona_queries))) {
        qid     <- persona_queries$query_id[q]
        qstring <- persona_queries$query_string[q]
        
        query_resolved  <- inject_near_me_location(qstring, reach, country,
                                                   region, postcode)
        profiled_prompt <- paste0(prefix_p, query_resolved)
        
        pq_ids   <- sprintf("PE_p%d_b%d_q%d_p%02d", pid, bid, qid, seq_len(reps))
        pq_prompts <- setNames(rep(profiled_prompt, reps), pq_ids)
        all_prompt_map <- c(all_prompt_map, as.list(pq_prompts))
        persona_prompt_map[[as.character(pid)]]$brand_ids_map[[as.character(bid)]]$query_ids[[as.character(qid)]] <- pq_ids
      }
    }
  }
  
  total_prompts <- length(all_prompt_map)
  message(sprintf("\n--- Phase 1 complete: %d total prompts ---", total_prompts))
  message(sprintf("   Brand: %d | Query: %d | Persona: %d",
                  length(all_prompt_map) - length(unlist(query_response_map, recursive=FALSE)) - 
                    length(unlist(persona_prompt_map, recursive=FALSE)),
                  sum(sapply(query_response_map, function(x) length(unlist(x$resolved_ids)))),
                  sum(sapply(persona_prompt_map, function(x) 
                    length(unlist(x$brand_ids_map))))))
  
  # ── Phase 2: ONE batch submission ─────────────────────────────
  message("\n--- Phase 2: Submitting single batch for everything ---")
  
  all_responses <- tryCatch(
    run_full_batch(
      all_prompt_map,
      model         = model,
      temperature   = 0.1,
      poll_interval = 60,
      max_wait_mins = 180
    ),
    error = function(e) {
      message(sprintf("Batch failed: %s — falling back to async", e$message))
      resps <- ask_chatgpt_async(unlist(all_prompt_map),
                                 model = model, temperature = 0.1)
      setNames(as.list(resps), names(all_prompt_map))
    }
  )
  
  # ── Phase 3: Score brands ──────────────────────────────────────
  message("\n--- Phase 3: Scoring brands ---")
  
  failed_brands <- c()
  
  for (i in seq_len(n_brands)) {
    row    <- user_brand_list[i, ]
    brand  <- row$brand_name
    lid    <- row$login_id
    bid    <- row$brand_id
    prefix <- sprintf("brand%d_user%d", i, lid)
    
    tryCatch({
      t0   <- Sys.time()
      ids  <- brand_prompt_map[[prefix]]
      resps <- lapply(ids, function(id) all_responses[[id]])
      
      idx  <- 0
      grab <- function(n) {
        r <- resps[(idx+1):(idx+n)]; idx <<- idx+n; unlist(r)
      }
      
      competitor_raw   <- grab(10); industry_raw     <- grab(10)
      description_raw  <- grab(10); founded_raw      <- grab(10)
      hq_raw           <- grab(10); ceo_raw          <- grab(10)
      industry_det_raw <- grab(10); parent_raw       <- grab(10)
      products_raw     <- grab(10); pricing_raw      <- grab(10)
      target_raw       <- grab(10); milestone_raw    <- grab(10)
      origin_raw       <- grab(10); employees_raw    <- grab(10)
      revenue_raw      <- grab(10); countries_raw    <- grab(10)
      best_brands_raw  <- grab(10); top_brands_raw   <- grab(10)
      leading_raw      <- grab(10); rec_brands_raw   <- grab(10)
      popular_raw      <- grab(10); major_raw        <- grab(10)
      
      get_data <- list(
        competitor_summary = competitor_raw %>%
          unlist() %>% str_split(";") %>% unlist() %>% str_trim() %>%
          tibble(competitor = .) %>% filter(nchar(competitor) > 0) %>%
          count(competitor, sort = TRUE, name = "mentions") %>% head(9),
        industry_summary = industry_raw %>%
          unlist() %>% tibble(industry = .) %>%
          mutate(industry_desc = substr(industry, 12 + nchar(brand),
                                        nchar(industry) - 10)) %>%
          filter(nchar(industry_desc) > 0) %>%
          count(industry_desc, sort = TRUE, name = "mentions") %>% head(1),
        comp_second_response_list = as.list(best_brands_raw),
        description_list          = as.list(description_raw),
        presence_data = list(
          top_brands        = list(data = as.list(top_brands_raw),  type = "text", question = "Top brands"),
          leading_brands    = list(data = as.list(leading_raw),     type = "text", question = "Leading brands"),
          rec_brands        = list(data = as.list(rec_brands_raw),  type = "text", question = "Recommended brands"),
          popular_companies = list(data = as.list(popular_raw),     type = "text", question = "Popular companies"),
          major_players     = list(data = as.list(major_raw),       type = "text", question = "Major players")
        ),
        perception_data = list(
          founded           = list(data = as.list(founded_raw),      type = "year",        question = "Founded"),
          hq                = list(data = as.list(hq_raw),           type = "location",    question = "HQ"),
          ceo               = list(data = as.list(ceo_raw),          type = "name",        question = "CEO"),
          industry          = list(data = as.list(industry_det_raw), type = "text",        question = "Industry"),
          parent_company    = list(data = as.list(parent_raw),       type = "categorical", question = "Parent"),
          products_services = list(data = as.list(products_raw),     type = "list",        question = "Products"),
          pricing           = list(data = as.list(pricing_raw),      type = "categorical", question = "Pricing"),
          target_market     = list(data = as.list(target_raw),       type = "text",        question = "Target"),
          milestone         = list(data = as.list(milestone_raw),    type = "text",        question = "Milestone"),
          start             = list(data = as.list(origin_raw),       type = "text",        question = "Origin"),
          num_employees     = list(data = as.list(employees_raw),    type = "numeric",     question = "Employees"),
          revenue           = list(data = as.list(revenue_raw),      type = "numeric",     question = "Revenue"),
          countries         = list(data = as.list(countries_raw),    type = "list",        question = "Countries")
        )
      )
      
      search_names <- get_brand_search_names(bid, brand, con)
      presence     <- calculate_presence_from_prompts(search_names, get_data$presence_data)
      perception   <- calculate_perception_from_prompts(brand, get_data)
      prestige     <- calculate_prestige_from_prompts(brand, get_data)
      
      upload_daily_refresh(con, brand, lid,
                           list(presence = presence,
                                perception = perception,
                                prestige = prestige),
                           Sys.Date())
      calc_daily_persistance(con, brand, lid, Sys.Date())
      calc_daily_airr(con, brand, lid, Sys.Date())
      
      message(sprintf("  \u2713 [%d/%d] %s / user %d (%.0fs)",
                      i, n_brands, brand, lid,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      
    }, error = function(e) {
      failed_brands <<- c(failed_brands, paste0(brand, "/user:", lid))
      message(sprintf("  \u2717 [%d/%d] %s: %s", i, n_brands, brand, e$message))
    })
  }
  
  # ── Phase 4: Score personas from cached responses ──────────────
  message("\n--- Phase 4: Scoring personas ---")
  
  for (p in seq_len(n_profiles)) {
    pid   <- active_profiles$profile_id[p]
    pname <- active_profiles$profile_name[p]
    lid   <- active_profiles$login_id[p]
    pmap  <- persona_prompt_map[[as.character(pid)]]
    
    message(sprintf("  [%d/%d] %s (login: %d)...",
                    p, n_profiles, pname, lid))
    
    tryCatch({
      
      for (b in seq_len(nrow(pmap$brands))) {
        bid        <- pmap$brands$brand_id[b]
        brand_name <- pmap$brands$brand_name[b]
        bmap       <- pmap$brand_ids_map[[as.character(bid)]]
        
        # Extract 80 brand-level responses
        brand_resps <- lapply(bmap$brand_ids, function(id) all_responses[[id]])
        
        presence_responses <- data.frame(
          prompt    = rep("profile_presence", 50),
          responses = sanitise_text(unlist(brand_resps[1:50])),
          stringsAsFactors = FALSE
        )
        perception_text <- sanitise_text(unlist(brand_resps[51:80]))
        
        presence_score   <- presence_prompt_calc(brand_name, presence_responses)
        sentiment_result <- analyze_sentiment_weighted(perception_text)
        perception_score <- sentiment_result$score
        prestige_score   <- calculate_prestige_from_prompts_sep(
          brand_name    = brand_name,
          brand_id      = bid,
          login_id      = lid,
          rel_responses = presence_responses,
          db_con        = con
        )
        
        presence_history <- dbGetQuery(con,
                                       "SELECT date, presence_score FROM fact_profile_brand_history
           WHERE profile_id = $1 AND brand_id = $2 ORDER BY date",
                                       params = list(pid, bid))
        
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
                  params = list(pid, bid, Sys.Date(),
                                presence_score, perception_score,
                                prestige_score, persistence_score, airr_score))
        
        message(sprintf("    \u2713 Brand %s scored for profile %d",
                        brand_name, pid))
        
        # Score persona queries for this brand
        for (qkey in names(bmap$query_ids)) {
          qid       <- as.integer(qkey)
          pq_ids    <- bmap$query_ids[[qkey]]
          pq_resps  <- lapply(pq_ids, function(id) all_responses[[id]])
          
          rel_responses <- data.frame(
            prompt    = rep("profile_query", 10),
            responses = sanitise_text(unlist(pq_resps)),
            stringsAsFactors = FALSE
          )
          
          pq_presence   <- presence_prompt_calc(brand_name, rel_responses)
          pq_prestige   <- calculate_prestige_from_prompts_sep(
            brand_name = brand_name, brand_id = bid, login_id = lid,
            rel_responses = rel_responses, db_con = con)
          pq_perception <- calculate_perception_from_prompts_sep(
            brand_name, bid, rel_responses)
          
          pq_hist <- dbGetQuery(con,
                                "SELECT date, presence_score FROM fact_profile_query_history
             WHERE profile_id=$1 AND brand_id=$2 AND query_id=$3
             ORDER BY date",
                                params = list(pid, bid, qid))
          
          pq_persistence <- if (nrow(pq_hist) < 5) pq_presence else
            calculate_daily_persistence_sep(pq_hist)
          
          pq_airr <- pq_presence   * AIRR_WEIGHTS$presence    +
            pq_perception * AIRR_WEIGHTS$perception   +
            pq_prestige   * AIRR_WEIGHTS$prestige      +
            pq_persistence* AIRR_WEIGHTS$persistence
          
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
                    params = list(pid, bid, qid, Sys.Date(),
                                  pq_presence, pq_perception,
                                  pq_prestige, pq_persistence, pq_airr))
          
          message(sprintf("    \u2713 Prompt %d scored for brand %s, profile %d",
                          qid, brand_name, pid))
        }
      }
      
    }, error = function(e) {
      message(sprintf("  \u2717 Persona %s failed: %s", pname, e$message))
    })
  }
  
  # ── Phase 5: Score queries from cached responses ───────────────
  message("\n--- Phase 5: Scoring queries ---")
  
  failed_queries <- c()
  
  for (q in seq_len(n_queries)) {
    qid     <- active_queries$query_id[q]
    qstring <- active_queries$query_string[q]
    qmap    <- query_response_map[[as.character(qid)]]
    
    if (is.null(qmap)) next
    
    tryCatch({
      message(sprintf("\n[%d/%d] Query ID %d: %s...",
                      q, n_queries, qid, substr(qstring, 1, 50)))
      
      bfq <- qmap$brands_for_query
      
      # Ensure dim_brand_query rows exist
      for (bid in unique(bfq$brand_id)) {
        dbExecute(con,
                  "INSERT INTO dim_brand_query (brand_id, query_id, date_added)
           VALUES ($1,$2,$3) ON CONFLICT (brand_id, query_id) DO NOTHING",
                  params = list(bid, qid, Sys.Date()))
      }
      
      for (b in seq_len(nrow(bfq))) {
        bid        <- bfq$brand_id[b]
        brand_name <- bfq$brand_name[b]
        lid        <- bfq$login_id[b]
        resolved   <- bfq$query_resolved[b]
        resp_ids   <- qmap$resolved_ids[[resolved]]
        
        rel_resp <- lapply(resp_ids, function(id) all_responses[[id]])
        
        search_names <- get_brand_search_names(bid, brand_name, con)
        
        tryCatch({
          presence_score <- presence_prompt_calc(search_names, rel_resp)
          
          prestige_score <- calculate_prestige_from_prompts_sep(
            brand_name = brand_name, brand_id = bid, login_id = lid,
            rel_responses = rel_resp, db_con = con)
          
          perception_score <- calculate_perception_from_prompts_sep(
            brand_name, bid, rel_resp)
          
          dbExecute(con, "
            INSERT INTO fact_query_history (
              brand_id, query_id, date, presence_score, perception_score,
              prestige_score, persistence_score, airr_score
            ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
            ON CONFLICT (brand_id, query_id, date) DO UPDATE SET
              presence_score    = EXCLUDED.presence_score,
              perception_score  = EXCLUDED.perception_score,
              prestige_score    = EXCLUDED.prestige_score,
              persistence_score = EXCLUDED.persistence_score,
              airr_score        = EXCLUDED.airr_score",
                    params = list(bid, qid, Sys.Date(),
                                  presence_score, perception_score,
                                  prestige_score, 0, 0))
          
          ph <- dbGetQuery(con,
                           "SELECT date, presence_score FROM fact_query_history
             WHERE brand_id=$1 AND date<=CURRENT_DATE AND query_id=$2
             ORDER BY date",
                           params = list(bid, qid))
          
          persistence_score <- if (nrow(ph) < 5) presence_score else
            calculate_daily_persistence_sep(ph)
          
          ts <- dbGetQuery(con,
                           "SELECT presence_score, perception_score, prestige_score
             FROM fact_query_history
             WHERE brand_id=$1 AND date=CURRENT_DATE AND query_id=$2",
                           params = list(bid, qid))
          
          airr <- ts$perception_score * AIRR_WEIGHTS$perception +
            ts$presence_score   * AIRR_WEIGHTS$presence   +
            ts$prestige_score   * AIRR_WEIGHTS$prestige   +
            persistence_score   * AIRR_WEIGHTS$persistence
          
          dbExecute(con,
                    "UPDATE fact_query_history
             SET persistence_score=$1, airr_score=$2
             WHERE brand_id=$3 AND date=CURRENT_DATE AND query_id=$4",
                    params = list(persistence_score, airr, bid, qid))
          
          message(sprintf("    \u2713 %s (user: %d)", brand_name, lid))
          
        }, error = function(e) {
          failed_queries <<- c(failed_queries,
                               paste0("brand:", bid, "/query:", qid))
          message(sprintf("    \u2717 %s: %s", brand_name, e$message))
        })
      }
    }, error = function(e) {
      failed_queries <<- c(failed_queries, paste0("query:", qid))
      message(sprintf("  \u2717 Query %d: %s", qid, e$message))
    })
  }
  
  message(sprintf(
    "\nAll scoring complete. Brand failures: %d | Query failures: %d",
    length(failed_brands), length(failed_queries)
  ))
  
  list(
    refresh_failed = failed_brands,
    prompt_failed  = failed_queries
  )
}

