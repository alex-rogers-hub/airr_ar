# ============================================
# Dashboard - User's brands with main highlighted
# ============================================

observe({
  cat("=== STATE ===\n")
  cat("logged_in:", rv$logged_in, "\n")
  cat("login_id:", rv$login_id, "\n")
  cat("brand_id:", rv$brand_id, "\n")
  cat("onboarding_complete:", rv$onboarding_complete, "\n")
})

# --- Reactive: Get all user's brand IDs (main + competitors) ---
user_all_brand_ids <- reactive({
  cat("=== user_all_brand_ids firing ===\n")
  req(rv$logged_in, rv$login_id)
  rv$brands_refresh
  cat("=== user_all_brand_ids: login_id =", rv$login_id, "===\n")
  
  req(rv$logged_in, rv$login_id)
  rv$brands_refresh
  
  query <- "
    SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    ORDER BY ubt.main_brand_flag DESC, b.brand_name
  "
  dbGetQuery(pool, query, params = list(rv$login_id))
})

# ============================================
# Auto-refresh dashboard while scores are being calculated
# ============================================

observe({
  req(rv$logged_in, rv$login_id)
  
  current_refresh <- rv$brands_refresh
  
  brands <- tryCatch(
    isolate(user_all_brand_ids()),
    error = function(e) NULL
  )
  
  if (is.null(brands) || nrow(brands) == 0) return()
  
  brand_ids    <- brands$brand_id
  n            <- length(brand_ids)
  placeholders <- paste0("$", 2:(n + 1), collapse = ", ")
  
  brands_with_scores <- tryCatch({
    result <- dbGetQuery(pool, sprintf("
      SELECT COUNT(DISTINCT brand_id) as cnt
      FROM fact_airr_history
      WHERE login_id = $1
        AND brand_id IN (%s)
        AND airr_score IS NOT NULL
    ", placeholders), params = as.list(c(rv$login_id, brand_ids)))
    result$cnt[1]
  }, error = function(e) 0)
  
  total_brands <- nrow(brands)
  
  # Only keep polling if scores have been missing for less than 30 minutes
  # Check when the user's brands were first added
  oldest_brand_added <- tryCatch({
    dbGetQuery(pool, "
      SELECT MIN(date_valid_from) as first_added
      FROM fact_user_brands_tracked
      WHERE login_id = $1
    ", params = list(rv$login_id))$first_added[1]
  }, error = function(e) NULL)
  
  minutes_since_setup <- if (!is.null(oldest_brand_added) && !is.na(oldest_brand_added)) {
    as.numeric(difftime(Sys.time(), oldest_brand_added, units = "mins"))
  } else {
    999  # Unknown — don't keep polling forever
  }
  
  if (isTRUE(brands_with_scores < total_brands) && minutes_since_setup < 30) {
    # Still waiting for scores, poll again in 8 seconds
    invalidateLater(8000, session)
    rv$brands_refresh <- isolate(rv$brands_refresh) + 1
  }
  # If scores are missing but setup was > 30 mins ago, stop polling —
  # the job either failed or will never complete
})

# --- Reactive: Latest scores for all user's brands ---
dash_latest_scores <- reactive({
  cat("=== dash_latest_scores firing ===\n")
  req(user_all_brand_ids())
  cat("=== dash_latest_scores: running query ===\n")
  
  req(user_all_brand_ids())
  
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id          # NEW
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids    <- brands$brand_id
  n            <- length(brand_ids)
  # $1 = login_id, $2..$n+1 = brand_ids
  placeholders <- paste0("$", 2:(n + 1), collapse = ", ")
  
  query <- sprintf("
    WITH latest_dates AS (
      SELECT brand_id, MAX(date) as latest_date
      FROM fact_airr_history
      WHERE login_id = $1                        -- NEW
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
      AND fa.login_id = $1                       -- NEW
    LEFT JOIN fact_presence_history fpres
      ON db.brand_id = fpres.brand_id AND fpres.date = ld.latest_date
      AND fpres.login_id = $1                    -- NEW
    LEFT JOIN fact_perception_history fperc
      ON db.brand_id = fperc.brand_id AND fperc.date = ld.latest_date
      AND fperc.login_id = $1                    -- NEW
    LEFT JOIN fact_prestige_history fprest
      ON db.brand_id = fprest.brand_id AND fprest.date = ld.latest_date
      AND fprest.login_id = $1                   -- NEW
    LEFT JOIN fact_persistence_history fpers
      ON db.brand_id = fpers.brand_id AND fpers.date = ld.latest_date
      AND fpers.login_id = $1                    -- NEW
    ORDER BY fa.airr_score DESC
  ", placeholders)
  
  # params: login_id first, then brand_ids
  result <- dbGetQuery(pool, query, params = as.list(c(login_id, brand_ids)))
  result <- result %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  cat("=== dash_latest_scores: query complete ===\n")
  result
})

# --- Reactive: Timeseries for all user's brands ---
dash_timeseries <- reactive({
  req(user_all_brand_ids())
  
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id          # NEW
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids    <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  query <- sprintf("
    SELECT db.brand_name, db.brand_id, fa.date, fa.airr_score
    FROM dim_brand db
    LEFT JOIN fact_airr_history fa
      ON db.brand_id = fa.brand_id
      AND fa.login_id = $1                       -- NEW
    WHERE db.brand_id IN (%s)
      AND fa.airr_score IS NOT NULL
    ORDER BY db.brand_name, fa.date
  ", placeholders)
  
  result <- dbGetQuery(pool, query, params = as.list(c(login_id, brand_ids)))
  result %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
})

# ============================================
# Score Cards - Combined as one UI output
# ============================================

output$dash_score_cards_row <- renderUI({
  
  scores <- tryCatch(dash_latest_scores(), error = function(e) NULL)
  
  # Check if we have brands but no scores yet (background still running)
  brands <- tryCatch(user_all_brand_ids(), error = function(e) NULL)
  has_brands_no_scores <- !is.null(brands) && nrow(brands) > 0 && 
    (is.null(scores) || nrow(scores) == 0)
  
  if (has_brands_no_scores) {
    return(
      fluidRow(
        column(
          width = 3,
          div(
            style = "background: white; border-radius: 16px; padding: 16px 20px;
             box-shadow: 0 2px 12px rgba(0,0,0,0.06);
             display: flex; flex-direction: column; gap: 12px;
             justify-content: center;",
            
            # Spinner row
            div(
              style = "display: flex; align-items: center; justify-content: space-around;",
              div(
                style = "text-align: center;",
                tags$i(class = "fa fa-spinner fa-spin",
                       style = "font-size: 20px; color: #667eea; margin-bottom: 8px;"),
                div(style = "font-size: 13px; font-weight: 600; color: #4a5568;",
                    "Setting up your dashboard"),
                div(style = "font-size: 11px; color: #a0aec0; margin-top: 4px;",
                    paste0(nrow(brands), " brand",
                           if (nrow(brands) != 1) "s" else "",
                           " being scored...")),
                div(style = "font-size: 11px; color: #cbd5e0; margin-top: 6px;",
                    "Scores will appear automatically when ready")
              )
            ),
            
            # Divider
            div(style = "height: 1px; background: #f0f0f0;"),
            
            # Industry row (still show industry even while calculating)
            # Industry row with optional low-presence nudge
            div(
              style = "display: flex; align-items: flex-start; gap: 8px;",
              div(
                style = "flex: 0 0 auto; width: 24px; height: 24px; border-radius: 6px;
             background: rgba(102,126,234,0.08); display: flex;
             align-items: center; justify-content: center; margin-top: 2px;",
                icon("industry", style = "font-size: 11px; color: #667eea;")
              ),
              div(
                style = "flex: 1; min-width: 0;",
                div(
                  style = "font-size: 10px; font-weight: 600; text-transform: uppercase;
               letter-spacing: 0.5px; color: #718096; margin-bottom: 2px;",
                  "Industry"
                ),
                div(
                  style = "font-size: 12px; font-weight: 600; color: #2d3748;
               white-space: normal; line-height: 1.3;",
                  {
                    ind <- tryCatch({
                      dbGetQuery(pool,
                                 "SELECT industry FROM fact_user_brands_tracked
             WHERE login_id = $1
               AND main_brand_flag = TRUE
               AND date_valid_from <= CURRENT_DATE
               AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
             LIMIT 1",
                                 params = list(rv$login_id))$industry[1]
                    }, error = function(e) NA)
                    if (is.na(ind) || is.null(ind) || !nzchar(ind %||% "")) "—" else ind
                  }
                ),
                
                # Low presence nudge — only shown when presence is near zero
                {
                  has_zero_presence <- tryCatch({
                    result <- dbGetQuery(pool, "
          SELECT COALESCE(AVG(overall_score), -1) as avg_presence
          FROM fact_presence_history
          WHERE login_id = $1
            AND date >= CURRENT_DATE - INTERVAL '7 days'",
                                         params = list(rv$login_id))
                    score <- result$avg_presence[1]
                    !is.na(score) && score >= 0 && score < 5
                  }, error = function(e) FALSE)
                  
                  if (has_zero_presence) {
                    
                    # Fetch reach payload for the pill buttons
                    reach_info <- tryCatch(
                      dbGetQuery(pool, "
      SELECT b.brand_id, b.brand_name, b.brand_reach, b.reach_country,
             b.reach_region, b.reach_postcode, ubt.industry
      FROM fact_user_brands_tracked ubt
      JOIN dim_brand b ON b.brand_id = ubt.brand_id
      WHERE ubt.login_id = $1
        AND ubt.main_brand_flag = TRUE
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
      LIMIT 1",
                                 params = list(rv$login_id)),
                      error = function(e) NULL
                    )
                    
                    industry_payload_pill <- if (!is.null(reach_info) && nrow(reach_info) > 0) {
                      jsonlite::toJSON(list(
                        brand_id   = reach_info$brand_id[1],
                        brand_name = reach_info$brand_name[1],
                        industry   = reach_info$industry[1] %||% ""
                      ), auto_unbox = TRUE)
                    } else NULL
                    
                    reach_payload_pill <- if (!is.null(reach_info) && nrow(reach_info) > 0) {
                      jsonlite::toJSON(list(
                        brand_reach    = reach_info$brand_reach[1]    %||% "global",
                        reach_country  = reach_info$reach_country[1]  %||% "",
                        reach_region   = reach_info$reach_region[1]   %||% "",
                        reach_postcode = reach_info$reach_postcode[1] %||% ""
                      ), auto_unbox = TRUE)
                    } else NULL
                    
                    div(
                      style = "margin-top: 6px;",
                      
                      # Clickable pill
                      div(
                        style = "display: inline-flex; align-items: center; gap: 5px;
               background: rgba(231,76,60,0.1); border: 1px solid rgba(231,76,60,0.3);
               border-radius: 20px; padding: 3px 10px;
               transition: all 0.15s ease; position: relative;",
                        class = "industry-nudge-pill",
                        
                        icon("triangle-exclamation",
                             style = "font-size: 10px; color: #E74C3C; flex-shrink: 0;"),
                        tags$span(
                          style = "font-size: 11px; font-weight: 600; color: #C0392B; white-space: nowrap;",
                          "Low presence — fix settings"
                        ),
                        
                        # Hover tooltip
                        tags$div(
                          class = "industry-nudge-tooltip",
                          style = "position: absolute; left: 0; top: calc(100% + 8px);
                 background: #F5F5F5; color: #1A1A1A;
                 font-size: 11px; font-weight: 400; line-height: 1.5;
                 padding: 10px 12px; border-radius: 8px;
                 box-shadow: 0 2px 12px rgba(0,0,0,0.15);
                 border: 1px solid #e2e8f0;
                 width: 240px; pointer-events: none;
                 opacity: 0; transition: opacity 0.15s ease;
                 z-index: 99999;",
                          div(style = "font-weight: 700; color: #C0392B; margin-bottom: 4px;",
                              "Your Presence score is very low"),
                          div(style = "color: #4a5568; margin-bottom: 8px;",
                              "A broad industry or wide reach stops AI models from connecting 
             your brand to relevant searches."),
                          div(style = "font-weight: 600; color: #667eea;",
                              "Go to Account to update \u2192")
                        )
                      ),
                      
                      # Inline action buttons below the pill
                      div(
                        style = "display: flex; gap: 6px; margin-top: 6px; flex-wrap: wrap;",
                        if (!is.null(industry_payload_pill)) {
                          tags$button(
                            style = "background: rgba(231,76,60,0.12); color: #C0392B;
                   border: 1px solid rgba(231,76,60,0.3);
                   border-radius: 6px; padding: 3px 10px;
                   font-size: 11px; font-weight: 600; cursor: pointer;",
                            onclick = sprintf(
                              "Shiny.setInputValue('edit_industry_btn', %s, {priority: 'event'})",
                              industry_payload_pill),
                            icon("industry", style = "font-size: 10px; margin-right: 4px;"),
                            "Fix Industry"
                          )
                        },
                        if (!is.null(reach_payload_pill)) {
                          tags$button(
                            style = "background: rgba(231,76,60,0.12); color: #C0392B;
                   border: 1px solid rgba(231,76,60,0.3);
                   border-radius: 6px; padding: 3px 10px;
                   font-size: 11px; font-weight: 600; cursor: pointer;",
                            onclick = sprintf(
                              "Shiny.setInputValue('edit_reach_btn', %s, {priority: 'event'})",
                              reach_payload_pill),
                            icon("globe", style = "font-size: 10px; margin-right: 4px;"),
                            "Fix Reach"
                          )
                        }
                      )
                    )
                  }
                }
              )
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "score-card-main",
            div(class = "score-label", "AiRR Score"),
            div(
              style = "margin: 15px 0;",
              tags$i(class = "fa fa-spinner fa-spin",
                     style = "font-size: 32px; color: #D4A843;")
            ),
            div(style = "font-size: 12px; opacity: 0.8;", "Calculating...")
          )
        ),
        div(
          class = "score-card-grid",
          div(class = "score-card-mini presence",
              `data-tooltip` = "Are you appearing?",
              div(class = "score-label", "Presence"),
              div(class = "score-value",
                  tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;"))),
          div(class = "score-card-mini perception",
              `data-tooltip` = "How are you described?",
              div(class = "score-label", "Perception"),
              div(class = "score-value",
                  tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;"))),
          div(class = "score-card-mini prestige",
              `data-tooltip` = "Are you recommended above competitors?",
              div(class = "score-label", "Prestige"),
              div(class = "score-value",
                  tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;"))),
          div(class = "score-card-mini persistence",
              `data-tooltip` = "Does your visibility hold over time?",
              div(class = "score-label", "Persistence"),
              div(class = "score-value",
                  tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;")))
        )
      )
    )
  }
  
  if (is.null(scores) || nrow(scores) == 0) {
    return(
      fluidRow(
        column(
          width = 12,
          div(
            style = "background: white; border-radius: 16px; padding: 40px; 
                     text-align: center; box-shadow: 0 2px 12px rgba(0,0,0,0.06);",
            icon("chart-line", class = "fa-3x", style = "color: #ccc; margin-bottom: 15px;"),
            h4("No score data available yet"),
            p(style = "color: #718096;", "Scores will appear once they've been calculated.")
          )
        )
      )
    )
  }
  
  # --- Normal score cards ---
  main_scores <- scores %>% filter(main_brand_flag == TRUE)
  comps       <- scores %>% filter(main_brand_flag == FALSE)
  
  airr_val  <- if (nrow(main_scores) > 0) round(main_scores$airr_score[1], 1)  else "—"
  airr_date <- if (nrow(main_scores) > 0) format(main_scores$date[1], "%b %d") else ""
  
  presence_val    <- if (nrow(main_scores) > 0) round(main_scores$presence_score[1], 1)    else "—"
  perception_val  <- if (nrow(main_scores) > 0) round(main_scores$perception_score[1], 1)  else "—"
  prestige_val    <- if (nrow(main_scores) > 0) round(main_scores$prestige_score[1], 1)    else "—"
  persistence_val <- if (nrow(main_scores) > 0) round(main_scores$persistence_score[1], 1) else "—"
  
  main_rank <- if (nrow(main_scores) > 0) {
    which(scores$brand_name == main_scores$brand_name[1])
  } else {
    "—"
  }
  
  fluidRow(
    # --- Col 1: Your Position ---
    column(
      width = 3,
      div(
        style = "background: white; border-radius: 16px; padding: 16px 20px;
             box-shadow: 0 2px 12px rgba(0,0,0,0.06);
             display: flex; flex-direction: column; gap: 12px; height: 100%;
             justify-content: center;",
        
        # Top row: rank + divider + competitors
        div(
          style = "display: flex; align-items: center; justify-content: space-around;",
          
          # Rank
          div(
            style = "text-align: center;",
            div(
              style = "font-size: 10px; font-weight: 600; text-transform: uppercase;
                   letter-spacing: 0.5px; color: #718096; margin-bottom: 4px;",
              "Position"
            ),
            div(
              style = "font-size: 36px; font-weight: 700; color: #2d3748; line-height: 1;",
              paste0("#", main_rank)
            ),
            div(
              style = "font-size: 11px; color: #a0aec0; margin-top: 3px;",
              paste0("of ", nrow(scores))
            )
          ),
          
          # Divider
          div(style = "width: 1px; height: 50px; background: #e2e8f0; flex-shrink: 0;"),
          
          # Competitors
          div(
            style = "text-align: center;",
            div(
              style = "font-size: 10px; font-weight: 600; text-transform: uppercase;
                   letter-spacing: 0.5px; color: #718096; margin-bottom: 4px;",
              "Competitors"
            ),
            div(
              style = "font-size: 36px; font-weight: 700; color: #667eea; line-height: 1;",
              nrow(comps)
            ),
            div(
              style = "font-size: 11px; color: #a0aec0; margin-top: 3px;",
              "tracked"
            )
          )
        ),
        
        # Divider line
        div(style = "height: 1px; background: #f0f0f0;"),
        
        # Industry row
        # Industry row with low-presence nudge
        div(
          style = "display: flex; align-items: flex-start; gap: 8px;",
          div(
            style = "flex: 0 0 auto; width: 24px; height: 24px; border-radius: 6px;
             background: rgba(102,126,234,0.08); display: flex;
             align-items: center; justify-content: center; margin-top: 2px;",
            icon("industry", style = "font-size: 11px; color: #667eea;")
          ),
          div(
            style = "flex: 1; min-width: 0;",
            div(
              style = "font-size: 10px; font-weight: 600; text-transform: uppercase;
               letter-spacing: 0.5px; color: #718096; margin-bottom: 2px;",
              "Industry"
            ),
            div(
              style = "font-size: 12px; font-weight: 600; color: #2d3748;
               white-space: normal; line-height: 1.3;",
              {
                ind <- tryCatch({
                  dbGetQuery(pool,
                             "SELECT industry FROM fact_user_brands_tracked
             WHERE login_id = $1
               AND main_brand_flag = TRUE
               AND date_valid_from <= CURRENT_DATE
               AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
             LIMIT 1",
                             params = list(rv$login_id))$industry[1]
                }, error = function(e) NA)
                if (is.na(ind) || is.null(ind) || !nzchar(ind %||% "")) "—" else ind
              }
            ),
            
            # Low presence nudge
            {
              has_zero_presence <- tryCatch({
                result <- dbGetQuery(pool, "
          SELECT COALESCE(AVG(overall_score), -1) as avg_presence
          FROM fact_presence_history
          WHERE login_id = $1
            AND date >= CURRENT_DATE - INTERVAL '7 days'",
                                     params = list(rv$login_id))
                score <- result$avg_presence[1]
                !is.na(score) && score >= 0 && score < 5
              }, error = function(e) FALSE)
              
              if (has_zero_presence) {
                div(
                  style = "margin-top: 6px;",
                  tags$div(
                    style = "display: inline-flex; align-items: center; gap: 5px;
                     background: rgba(231,76,60,0.1); border: 1px solid rgba(231,76,60,0.3);
                     border-radius: 20px; padding: 3px 10px; cursor: pointer;
                     transition: all 0.15s ease; position: relative;",
                    class  = "industry-nudge-pill",
                    onclick = "Shiny.setInputValue('nav_to_account', Math.random(), {priority: 'event'})",
                    
                    icon("triangle-exclamation",
                         style = "font-size: 10px; color: #E74C3C; flex-shrink: 0;"),
                    tags$span(
                      style = "font-size: 11px; font-weight: 600; color: #C0392B;
                       white-space: nowrap;",
                      "Industry too broad?"
                    ),
                    icon("arrow-right",
                         style = "font-size: 9px; color: #E74C3C; flex-shrink: 0;"),
                    
                    # Hover tooltip
                    tags$div(
                      class = "industry-nudge-tooltip",
                      style = "position: absolute; left: 0; top: calc(100% + 8px);
                       background: #F5F5F5; color: #1A1A1A;
                       font-size: 11px; font-weight: 400; line-height: 1.5;
                       padding: 10px 12px; border-radius: 8px;
                       box-shadow: 0 2px 12px rgba(0,0,0,0.15);
                       border: 1px solid #e2e8f0; width: 230px;
                       pointer-events: none; opacity: 0;
                       transition: opacity 0.15s ease; z-index: 99999;",
                      div(
                        style = "font-weight: 700; color: #C0392B; margin-bottom: 4px;",
                        "Your Presence score is very low"
                      ),
                      div(
                        style = "color: #4a5568;",
                        "A broad industry stops AI models from connecting your brand
                 to relevant searches. Try something more specific —",
                        tags$em(" e.g. \"Connected Fitness Hardware\" instead of \"Fitness\".")
                      ),
                      div(
                        style = "margin-top: 6px; font-weight: 600; color: #667eea;",
                        "Click to update your industry →"
                      )
                    )
                  )
                )
              }
            }
          )
        )
      )
    ),
    
    # --- Col 2: AiRR Score ---
    column(
      width = 4,
      div(
        class = "score-card-main",
        div(class = "score-label", "AiRR Score"),
        div(class = "score-value", airr_val),
        div(style = "font-size: 12px; opacity: 0.8; margin-top: 5px;",
            if (airr_date != "") paste("as of", airr_date) else "")
      )
    ),
    
    # --- Col 3: Four sub-scores ---
    column(
      width = 5,
      div(
        class = "score-card-grid",
        div(class = "score-card-mini presence",
            `data-tooltip` = "Are you appearing?",
            div(class = "score-label", "Presence"),
            div(class = "score-value", presence_val)),
        div(class = "score-card-mini perception",
            `data-tooltip` = "How are you described?",
            div(class = "score-label", "Perception"),
            div(class = "score-value", perception_val)),
        div(class = "score-card-mini prestige",
            `data-tooltip` = "Are you recommended above competitors?",
            div(class = "score-label", "Prestige"),
            div(class = "score-value", prestige_val)),
        div(class = "score-card-mini persistence",
            `data-tooltip` = "Does your visibility hold over time?",
            div(class = "score-label", "Persistence"),
            div(class = "score-value", persistence_val))
      )
    )
  )
})

# ============================================
# Professional chart theme helper
# ============================================

plotly_pro_layout <- function(p, y_title = "", show_legend = TRUE) {
  p %>% layout(
    xaxis = list(
      title      = "",
      showgrid   = FALSE,
      linecolor  = "#e2e8f0",
      tickfont   = list(size = 11, color = "#9E9E9E", family = "Inter"),
      zeroline   = FALSE
    ),
    yaxis = list(
      title    = list(text = y_title,
                      font = list(size = 12, color = "#9E9E9E", family = "Inter")),
      showgrid  = TRUE,
      gridcolor = "rgba(0, 0, 0, 0.05)",
      gridwidth = 1,
      linecolor = "#e2e8f0",
      tickfont  = list(size = 11, color = "#9E9E9E", family = "Inter"),
      zeroline  = FALSE
    ),
    hovermode    = "closest",        # <-- changed from 'x unified'
    hoverlabel   = list(
      bgcolor    = "#1A1A1A",
      bordercolor = "#D4A843",
      font       = list(size = 12, color = "#F5F5F0", family = "Inter")
    ),
    plot_bgcolor  = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    margin        = list(l = 50, r = 20, t = 10, b = 40),
    legend = list(
      orientation = "h",
      yanchor     = "top",
      y           = -0.15,
      xanchor     = "center",
      x           = 0.5,
      font        = list(size = 11, color = "#9E9E9E", family = "Inter"),
      bgcolor     = "rgba(0,0,0,0)"
    ),
    showlegend = show_legend,
    font       = list(family = "Inter")
  ) %>% config(displayModeBar = FALSE)
}

# ============================================
# Chart helper with main brand highlighted
# ============================================

create_dash_chart <- function(data, score_col, y_title, main_brand_name) {
  
  main_data <- data %>% filter(main_brand_flag == TRUE)
  comp_data <- data %>% filter(main_brand_flag == FALSE)
  
  comp_colours <- c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6", 
                    "#E67E22", "#1ABC9C", "#34495E", "#F39C12",
                    "#16A085", "#C0392B")
  
  # ── Determine top 4 brands by most recent score ──────────────────────
  latest_by_brand <- data %>%
    group_by(brand_name) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    arrange(desc(.data[[score_col]])) %>%
    mutate(rank = row_number())
  
  top4_brands <- latest_by_brand %>%
    filter(rank <= 4) %>%
    pull(brand_name)
  
  p <- plot_ly()
  
  if (nrow(comp_data) > 0) {
    comp_brands <- unique(comp_data$brand_name)
    for (idx in seq_along(comp_brands)) {
      bn  <- comp_brands[idx]
      bd  <- comp_data %>% filter(brand_name == bn)
      col <- comp_colours[((idx - 1) %% length(comp_colours)) + 1]
      is_top4 <- bn %in% top4_brands
      
      p <- p %>% add_trace(
        data = bd,
        x    = ~date,
        y    = as.formula(paste0("~", score_col)),
        type = 'scatter',
        mode = 'lines+markers',
        name = bn,
        line = list(
          width = 2,
          dash  = if (is_top4) "solid" else "dot",
          shape = "spline",
          color = col
        ),
        marker = list(size = 4, color = col),
        opacity = 0.7,
        hovertemplate = paste0(
          "<b>", bn, "</b><br>",
          "%{x|%d %b %Y}<br>",
          y_title, ": <b>%{y:.1f}</b>",
          "<extra></extra>"
        )
      )
    }
  }
  
  if (nrow(main_data) > 0) {
    is_top4 <- main_brand_name %in% top4_brands
    
    p <- p %>% add_trace(
      data = main_data,
      x    = ~date,
      y    = as.formula(paste0("~", score_col)),
      type = 'scatter',
      mode = 'lines+markers',
      name = paste0("★ ", main_brand_name),
      line = list(
        width = 3.5,
        dash  = if (is_top4) "solid" else "dot",
        color = '#D4A843',
        shape = "spline"
      ),
      marker = list(
        size = 7,
        color = '#D4A843',
        line = list(color = '#1A1A1A', width = 1.5)
      ),
      hovertemplate = paste0(
        "<b>★ ", main_brand_name, "</b><br>",
        "%{x|%d %b %Y}<br>",
        y_title, ": <b>%{y:.1f}</b>",
        "<extra></extra>"
      )
    )
  }
  
  plotly_pro_layout(p, y_title, show_legend = TRUE)
}

# ============================================
# Score Trend Charts
# ============================================

output$dash_chart_airr <- renderPlotly({
  req(dash_timeseries())
  create_dash_chart(dash_timeseries(), "airr_score", "AIRR Score", rv$brand_name)
})

output$dash_chart_presence <- renderPlotly({
  req(user_all_brand_ids())
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.overall_score as presence_score
    FROM dim_brand db
    LEFT JOIN fact_presence_history fph
      ON db.brand_id = fph.brand_id AND fph.login_id = $1
    WHERE db.brand_id IN (%s) AND fph.overall_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>%
    left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  
  create_dash_chart(data, "presence_score", "Presence Score", rv$brand_name)
})

output$dash_chart_perception <- renderPlotly({
  req(user_all_brand_ids())
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.perception_score
    FROM dim_brand db
    LEFT JOIN fact_perception_history fph
      ON db.brand_id = fph.brand_id AND fph.login_id = $1
    WHERE db.brand_id IN (%s) AND fph.perception_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>%
    left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  
  create_dash_chart(data, "perception_score", "Perception Score", rv$brand_name)
})

output$dash_chart_prestige <- renderPlotly({
  req(user_all_brand_ids())
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.prestige_score
    FROM dim_brand db
    LEFT JOIN fact_prestige_history fph
      ON db.brand_id = fph.brand_id AND fph.login_id = $1
    WHERE db.brand_id IN (%s) AND fph.prestige_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>%
    left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  
  create_dash_chart(data, "prestige_score", "Prestige Score", rv$brand_name)
})

output$dash_chart_persistence <- renderPlotly({
  req(user_all_brand_ids())
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.persistence_score
    FROM dim_brand db
    LEFT JOIN fact_persistence_history fph
      ON db.brand_id = fph.brand_id AND fph.login_id = $1
    WHERE db.brand_id IN (%s) AND fph.persistence_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>%
    left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  
  create_dash_chart(data, "persistence_score", "Persistence Score", rv$brand_name)
})

# ============================================
# Spider Chart
# ============================================

output$dash_spider_compare <- renderPlotly({
  req(dash_latest_scores())
  
  data <- dash_latest_scores()
  if (nrow(data) == 0) return(NULL)
  
  comp_colours <- c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6", 
                    "#E67E22", "#1ABC9C", "#34495E", "#F39C12",
                    "#16A085", "#C0392B")
  
  p <- plot_ly(type = 'scatterpolar')
  
  comp_data <- data %>% filter(main_brand_flag == FALSE)
  for (i in seq_len(nrow(comp_data))) {
    col <- comp_colours[((i - 1) %% length(comp_colours)) + 1]
    
    p <- p %>% add_trace(
      r = c(comp_data$presence_score[i], comp_data$perception_score[i],
            comp_data$prestige_score[i], comp_data$persistence_score[i],
            comp_data$presence_score[i]),
      theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
      name = comp_data$brand_name[i],
      fill = 'toself',
      fillcolor = paste0(col, '15'),
      line = list(width = 2, color = col),
      marker = list(size = 5, color = col),
      opacity = 0.7,
      hovertemplate = paste0(
        '<b>', comp_data$brand_name[i], '</b><br>',
        '%{theta}: %{r:.1f}<extra></extra>')
    )
  }
  
  main_data <- data %>% filter(main_brand_flag == TRUE)
  if (nrow(main_data) > 0) {
    p <- p %>% add_trace(
      r = c(main_data$presence_score[1], main_data$perception_score[1],
            main_data$prestige_score[1], main_data$persistence_score[1],
            main_data$presence_score[1]),
      theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
      name = paste0("★ ", main_data$brand_name[1]),
      fill = 'toself',
      fillcolor = 'rgba(212, 168, 67, 0.15)',
      line = list(width = 3, color = '#D4A843'),
      marker = list(size = 8, color = '#D4A843'),
      hovertemplate = paste0(
        '<b>', main_data$brand_name[1], '</b><br>',
        '%{theta}: %{r:.1f}<extra></extra>')
    )
  }
  
  p %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE, range = c(0, 100),
        showline = FALSE, showticklabels = TRUE,
        gridcolor = "rgba(0, 0, 0, 0.06)",
        tickfont = list(size = 10, color = "#9E9E9E", family = "Inter")
      ),
      angularaxis = list(
        showline = FALSE,
        gridcolor = "rgba(0, 0, 0, 0.06)",
        tickfont = list(size = 12, color = "#4a5568", family = "Inter")
      ),
      bgcolor = "rgba(0,0,0,0)"
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      yanchor = "top",
      y = -0.15,
      xanchor = "center",
      x = 0.5,
      font = list(size = 11, color = "#9E9E9E", family = "Inter"),
      bgcolor = "rgba(0,0,0,0)"
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 60, r = 60, t = 30, b = 50),
    font = list(family = "Inter")
  ) %>% config(displayModeBar = FALSE)
})

# ============================================
# Rankings Table (highlighted main brand)
# ============================================

output$dash_rankings_table <- renderUI({
  req(dash_latest_scores())
  
  data <- dash_latest_scores() %>%
    arrange(desc(airr_score)) %>%
    mutate(rank = row_number())
  
  if (nrow(data) == 0) {
    return(div(
      style = "text-align: center; padding: 40px; color: #a0aec0;",
      icon("chart-bar", class = "fa-3x", style = "margin-bottom: 15px;"),
      h4("No brand data yet"),
      p("Add competitors to see rankings")
    ))
  }
  
  score_color <- function(val) {
    if (is.na(val)) return("#cbd5e0")
    if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
  }
  
  rows <- lapply(1:nrow(data), function(i) {
    row <- data[i, ]
    is_main <- isTRUE(row$main_brand_flag)
    
    rank_style <- if (i == 1) {
      "background: linear-gradient(135deg, #f6d365, #fda085); color: white;"
    } else if (i == 2) {
      "background: linear-gradient(135deg, #c0c0c0, #e0e0e0); color: #555;"
    } else if (i == 3) {
      "background: linear-gradient(135deg, #cd7f32, #e6a566); color: white;"
    } else {
      "background: #edf2f7; color: #718096;"
    }
    
    row_bg <- if (is_main) {
      "background: linear-gradient(90deg, rgba(102,126,234,0.08) 0%, rgba(102,126,234,0.03) 100%); 
       border-left: 4px solid #667eea;"
    } else {
      "border-left: 4px solid transparent;"
    }
    
    airr_val <- round(row$airr_score, 1)
    
    score_cell <- function(val) {
      div(
        style = "flex: 0 0 52px; text-align: center; font-size: 12px;
             font-weight: 600; color: #4a5568;",
        round(val, 0)
      )
    }
    
    div(
      style = paste0(
        "display: flex; align-items: center; padding: 12px 16px; ",
        "border-bottom: 1px solid #f0f0f0; transition: background 0.2s; ",
        row_bg
      ),
      class = "leaderboard-row",
      
      div(
        style = paste0(
          "flex: 0 0 36px; height: 36px; border-radius: 50%; display: flex; ",
          "align-items: center; justify-content: center; font-weight: 700; ",
          "font-size: 14px; margin-right: 16px; ", rank_style
        ),
        i
      ),
      
      div(
        style = "flex: 1; text-align: center; padding: 0 20px;",
        div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          tags$span(
            style = paste0(
              "font-weight: ", if (is_main) "700" else "500", "; ",
              "font-size: 16px; color: #2d3748;"
            ),
            row$brand_name
          ),
          if (is_main) {
            tags$span(
              style = "background: #667eea; color: white; font-size: 9px; padding: 2px 8px; 
                       border-radius: 10px; font-weight: 600; letter-spacing: 0.5px;",
              "YOUR BRAND"
            )
          }
        )
      ),
      
      div(style = "width: 1px; height: 32px; background: #2d3748; opacity: 0.15; margin: 0 8px;"),
      
      div(
        style = paste0(
          "flex: 0 0 90px; text-align: center; font-size: 24px; font-weight: 800; color: ", 
          score_color(airr_val), ";"
        ),
        airr_val
      ),
      
      div(style = "width: 1px; height: 32px; background: #2d3748; opacity: 0.15; margin: 0 8px;"),
      
      score_cell(row$presence_score),
      score_cell(row$perception_score),
      score_cell(row$prestige_score),
      score_cell(row$persistence_score)
    )
  })
  
  div(
    style = "border-radius: 10px; overflow: hidden; border: 1px solid #e2e8f0;",
    
    div(
      style = "display: flex; align-items: center; padding: 14px 16px; 
               background: linear-gradient(135deg, #1a202c 0%, #2d3748 100%);",
      
      div(style = "flex: 0 0 36px; margin-right: 16px;"),
      
      div(
        style = "flex: 1; text-align: center;",
        tags$span(style = "color: #a0aec0; font-weight: 600; font-size: 11px; 
                           text-transform: uppercase; letter-spacing: 1.5px;", "Brand")
      ),
      
      div(style = "width: 1px; height: 20px; background: #4a5568; margin: 0 8px;"),
      
      div(
        style = "flex: 0 0 90px; text-align: center;",
        tags$span(style = "color: #a0aec0; font-weight: 700; font-size: 12px; 
                   text-transform: uppercase; letter-spacing: 1px;", "AiRR")
      ),
      
      div(style = "width: 1px; height: 20px; background: #4a5568; margin: 0 8px;"),
      
      div(
        style = "flex: 0 0 80px; text-align: center;",
        tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                           text-transform: uppercase; letter-spacing: 0.8px;", "Presence")
      ),
      div(
        style = "flex: 0 0 80px; text-align: center;",
        tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                           text-transform: uppercase; letter-spacing: 0.8px;", "Perception")
      ),
      div(
        style = "flex: 0 0 80px; text-align: center;",
        tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                           text-transform: uppercase; letter-spacing: 0.8px;", "Prestige")
      ),
      div(
        style = "flex: 0 0 80px; text-align: center;",
        tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                           text-transform: uppercase; letter-spacing: 0.8px;", "Persistence")
      )
    ),
    
    do.call(tagList, rows)
  )
})

# ============================================
# Dashboard Query Dropdown
# ============================================

observe({
  req(rv$logged_in, rv$login_id)
  rv$queries_refresh
  
  queries <- get_user_tracked_queries(rv$login_id)
  
  if (nrow(queries) == 0) {
    updateSelectInput(session, "dash_query_select",
                      choices = c("No prompts tracked" = ""))
    return()
  }
  
  query_choices <- setNames(queries$query_string, queries$query_string)
  
  updateSelectInput(session, "dash_query_select",
                    choices = query_choices,
                    selected = query_choices[1])
})

# ============================================
# Dashboard Query Timeseries
# ============================================

dash_query_timeseries <- reactive({
  req(input$dash_query_select, input$dash_query_select != "")
  req(user_all_brand_ids())
  
  brands <- user_all_brand_ids()
  brand_ids <- brands$brand_id
  n_brands <- length(brand_ids)
  
  brand_placeholders <- paste0("$", 2:(n_brands + 1), collapse = ", ")
  
  query <- sprintf("
    SELECT db.brand_name, db.brand_id, fqh.date,
           fqh.airr_score, fqh.presence_score, fqh.perception_score,
           fqh.prestige_score, fqh.persistence_score
    FROM fact_query_history fqh
    INNER JOIN dim_brand db ON fqh.brand_id = db.brand_id
    INNER JOIN dim_query dq ON fqh.query_id = dq.query_id
    WHERE dq.query_string = $1
      AND fqh.brand_id IN (%s)
      AND fqh.airr_score IS NOT NULL
    ORDER BY db.brand_name, fqh.date
  ", brand_placeholders)
  
  result <- dbGetQuery(pool, query, params = as.list(c(input$dash_query_select, brand_ids)))
  result %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
})

# ============================================
# Dashboard Query Table
# ============================================

output$dash_query_table <- renderDT({
  req(dash_query_timeseries())
  
  data <- dash_query_timeseries()
  if (nrow(data) == 0) return(NULL)
  
  latest <- data %>%
    group_by(brand_name, main_brand_flag) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(brand_name = ifelse(main_brand_flag, paste0("⭐ ", brand_name), brand_name)) %>%
    arrange(desc(airr_score)) %>%
    select(Brand = brand_name, `AIRR Score` = airr_score,
           Presence = presence_score, Perception = perception_score,
           Prestige = prestige_score, Persistence = persistence_score,
           Date = date)
  
  datatable(
    latest,
    options = list(
      pageLength = 20,
      dom = 'tip',
      ordering = TRUE,
      scrollX = TRUE,
      rowCallback = JS(
        "function(row, data) {
     if (data[0].indexOf('⭐') > -1) {
       $(row).css('background-color', 'rgba(212, 168, 67, 0.08)');
       $(row).css('font-weight', '600');
     }
   }"
      )
    ),
    rownames = FALSE,
    class = 'cell-border'
  ) %>%
    formatRound(c('AIRR Score', 'Presence', 'Perception', 'Prestige', 'Persistence'), 1) %>%
    formatDate('Date', 'toDateString')
})

# ============================================
# Dashboard Query Charts
# ============================================

output$dash_query_chart_airr <- renderPlotly({
  req(dash_query_timeseries())
  create_dash_chart(dash_query_timeseries(), "airr_score", "AIRR Score", rv$brand_name)
})

output$dash_query_chart_presence <- renderPlotly({
  req(dash_query_timeseries())
  create_dash_chart(dash_query_timeseries(), "presence_score", "Presence Score", rv$brand_name)
})

output$dash_query_chart_perception <- renderPlotly({
  req(dash_query_timeseries())
  create_dash_chart(dash_query_timeseries(), "perception_score", "Perception Score", rv$brand_name)
})

output$dash_query_chart_prestige <- renderPlotly({
  req(dash_query_timeseries())
  create_dash_chart(dash_query_timeseries(), "prestige_score", "Prestige Score", rv$brand_name)
})

output$dash_query_chart_persistence <- renderPlotly({
  req(dash_query_timeseries())
  create_dash_chart(dash_query_timeseries(), "persistence_score", "Persistence Score", rv$brand_name)
})

# ============================================
# Download Handlers
# ============================================

output$download_brand_data <- downloadHandler(
  filename = function() paste0("airr_brand_data_", Sys.Date(), ".csv"),
  content = function(file) {
    req(user_all_brand_ids())
    brands    <- user_all_brand_ids()
    login_id  <- rv$login_id
    brand_ids <- brands$brand_id
    placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
    
    # --- Overall scores ---
    overall <- dbGetQuery(pool, sprintf("
      SELECT 
        db.brand_name,
        fa.date,
        'Overall'          AS persona,
        fa.airr_score,
        fpres.overall_score   AS presence_score,
        fperc.perception_score,
        fprest.prestige_score,
        fpers.persistence_score
      FROM dim_brand db
      LEFT JOIN fact_airr_history fa
        ON db.brand_id = fa.brand_id AND fa.login_id = $1
      LEFT JOIN fact_presence_history fpres
        ON db.brand_id = fpres.brand_id
        AND fpres.date = fa.date AND fpres.login_id = $1
      LEFT JOIN fact_perception_history fperc
        ON db.brand_id = fperc.brand_id
        AND fperc.date = fa.date AND fperc.login_id = $1
      LEFT JOIN fact_prestige_history fprest
        ON db.brand_id = fprest.brand_id
        AND fprest.date = fa.date AND fprest.login_id = $1
      LEFT JOIN fact_persistence_history fpers
        ON db.brand_id = fpers.brand_id
        AND fpers.date = fa.date AND fpers.login_id = $1
      WHERE db.brand_id IN (%s)
        AND fa.airr_score IS NOT NULL
      ORDER BY db.brand_name, fa.date
    ", placeholders), params = as.list(c(login_id, brand_ids)))
    
    # --- Persona scores (Enterprise only) ---
    persona_data <- tryCatch({
      
      active_personas <- dbGetQuery(pool, "
        SELECT upt.profile_id, dcp.profile_name
        FROM fact_user_profiles_tracked upt
        JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
        WHERE upt.login_id = $1
          AND upt.date_valid_from <= CURRENT_DATE
          AND (upt.date_valid_to IS NULL OR upt.date_valid_to >= CURRENT_DATE)
        ORDER BY dcp.profile_name
      ", params = list(login_id))
      
      if (nrow(active_personas) == 0) return(NULL)
      
      persona_rows <- lapply(1:nrow(active_personas), function(p) {
        pid   <- active_personas$profile_id[p]
        pname <- active_personas$profile_name[p]
        
        dbGetQuery(pool, sprintf("
          SELECT
            db.brand_name,
            fpbh.date,
            $2                 AS persona,
            fpbh.airr_score,
            fpbh.presence_score,
            fpbh.perception_score,
            fpbh.prestige_score,
            fpbh.persistence_score
          FROM fact_profile_brand_history fpbh
          JOIN dim_brand db ON db.brand_id = fpbh.brand_id
          WHERE fpbh.profile_id = $1
            AND fpbh.brand_id IN (%s)
            AND fpbh.airr_score IS NOT NULL
          ORDER BY db.brand_name, fpbh.date
        ", placeholders), params = as.list(c(pid, pname, brand_ids)))
      })
      
      bind_rows(persona_rows)
      
    }, error = function(e) NULL)
    
    # --- Combine and write ---
    combined <- if (!is.null(persona_data) && nrow(persona_data) > 0) {
      bind_rows(overall, persona_data) %>%
        arrange(brand_name, persona, date)
    } else {
      overall
    }
    
    write.csv(combined, file, row.names = FALSE)
  }
)

output$download_rankings_data <- downloadHandler(
  filename = function() {
    paste0("airr_rankings_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(dash_latest_scores())
    write.csv(dash_latest_scores() %>%
                select(-brand_id, -main_brand_flag), file, row.names = FALSE)
  }
)

output$download_prompt_data <- downloadHandler(
  filename = function() paste0("airr_prompt_data_", Sys.Date(), ".csv"),
  content = function(file) {
    req(user_all_brand_ids())
    
    brands    <- user_all_brand_ids()
    login_id  <- rv$login_id
    brand_ids <- brands$brand_id
    
    # Get all user's tracked queries
    user_queries <- get_user_tracked_queries(login_id)
    
    if (nrow(user_queries) == 0) {
      write.csv(data.frame(), file, row.names = FALSE)
      return()
    }
    
    # --- Overall prompt scores — one query per brand/query combo ---
    overall <- tryCatch({
      
      brand_ph <- paste0("$", seq_along(brand_ids), collapse = ", ")
      query_ph <- paste0("$", (length(brand_ids) + 1):(length(brand_ids) + nrow(user_queries)),
                         collapse = ", ")
      
      dbGetQuery(pool, sprintf("
        SELECT
          db.brand_name,
          fqh.date,
          'Overall'            AS persona,
          dq.query_string      AS prompt,
          fqh.airr_score,
          fqh.presence_score,
          fqh.perception_score,
          fqh.prestige_score,
          fqh.persistence_score
        FROM fact_query_history fqh
        JOIN dim_brand db ON db.brand_id = fqh.brand_id
        JOIN dim_query dq  ON dq.query_id  = fqh.query_id
        WHERE fqh.brand_id IN (%s)
          AND fqh.query_id  IN (%s)
          AND fqh.airr_score IS NOT NULL
        ORDER BY dq.query_string, db.brand_name, fqh.date
      ", brand_ph, query_ph),
                 params = as.list(c(brand_ids, user_queries$query_id)))
      
    }, error = function(e) {
      message("Overall prompt export error: ", e$message)
      data.frame()
    })
    
    # --- Persona prompt scores ---
    persona_data <- tryCatch({
      
      active_personas <- dbGetQuery(pool, "
        SELECT upt.profile_id, dcp.profile_name
        FROM fact_user_profiles_tracked upt
        JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
        WHERE upt.login_id = $1
          AND upt.date_valid_from <= CURRENT_DATE
          AND (upt.date_valid_to IS NULL OR upt.date_valid_to >= CURRENT_DATE)
        ORDER BY dcp.profile_name
      ", params = list(login_id))
      
      if (nrow(active_personas) == 0) return(NULL)
      
      persona_rows <- lapply(1:nrow(active_personas), function(p) {
        pid   <- active_personas$profile_id[p]
        pname <- active_personas$profile_name[p]
        
        brand_ph <- paste0("$", 3:(length(brand_ids) + 2), collapse = ", ")
        query_ph <- paste0("$", (length(brand_ids) + 3):(length(brand_ids) + nrow(user_queries) + 2),
                           collapse = ", ")
        
        tryCatch(
          dbGetQuery(pool, sprintf("
            SELECT
              db.brand_name,
              fpqh.date,
              $2                  AS persona,
              dq.query_string     AS prompt,
              fpqh.airr_score,
              fpqh.presence_score,
              fpqh.perception_score,
              fpqh.prestige_score,
              fpqh.persistence_score
            FROM fact_profile_query_history fpqh
            JOIN dim_brand db ON db.brand_id = fpqh.brand_id
            JOIN dim_query dq  ON dq.query_id  = fpqh.query_id
            WHERE fpqh.profile_id = $1
              AND fpqh.brand_id  IN (%s)
              AND fpqh.query_id  IN (%s)
              AND fpqh.airr_score IS NOT NULL
            ORDER BY dq.query_string, db.brand_name, fpqh.date
          ", brand_ph, query_ph),
                     params = as.list(c(pid, pname, brand_ids, user_queries$query_id))),
          error = function(e) {
            message("Persona prompt export error for ", pname, ": ", e$message)
            data.frame()
          }
        )
      })
      
      bind_rows(persona_rows)
      
    }, error = function(e) {
      message("Persona section error: ", e$message)
      NULL
    })
    
    # --- Combine and write ---
    combined <- if (!is.null(persona_data) && nrow(persona_data) > 0) {
      bind_rows(overall, persona_data) %>%
        arrange(prompt, brand_name, persona, date)
    } else {
      overall %>% arrange(prompt, brand_name, date)
    }
    
    write.csv(combined, file, row.names = FALSE)
  }
)

# ============================================
# Prompt Overview — Spider Chart
# ============================================

output$dash_query_spider <- renderPlotly({
  req(dash_query_timeseries())
  
  data <- dash_query_timeseries()
  if (nrow(data) == 0) return(NULL)
  
  latest <- data %>%
    group_by(brand_name, brand_id, main_brand_flag) %>%
    filter(date == max(date)) %>%
    ungroup()
  
  p <- plot_ly(type = 'scatterpolar', fill = 'toself')
  
  comp_data <- latest %>% filter(main_brand_flag == FALSE)
  for (i in seq_len(nrow(comp_data))) {
    p <- p %>% add_trace(
      r = c(comp_data$presence_score[i], comp_data$perception_score[i],
            comp_data$prestige_score[i], comp_data$persistence_score[i],
            comp_data$presence_score[i]),
      theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
      name = comp_data$brand_name[i],
      line = list(width = 1.5, dash = "dot"),
      marker = list(size = 5),
      opacity = 0.5,
      fillcolor = 'rgba(200, 200, 200, 0.1)',
      hovertemplate = '<b>%{fullData.name}</b><br>%{theta}: %{r:.1f}<br><extra></extra>'
    )
  }
  
  main_data <- latest %>% filter(main_brand_flag == TRUE)
  if (nrow(main_data) > 0) {
    p <- p %>% add_trace(
      r = c(main_data$presence_score[1], main_data$perception_score[1],
            main_data$prestige_score[1], main_data$persistence_score[1],
            main_data$presence_score[1]),
      theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
      name = paste0("⭐ ", main_data$brand_name[1]),
      line = list(width = 4, color = '#667eea'),
      marker = list(size = 10, color = '#667eea'),
      fillcolor = 'rgba(102, 126, 234, 0.3)',
      hovertemplate = paste0('<b>⭐ ', main_data$brand_name[1], '</b><br>%{theta}: %{r:.1f}<br><extra></extra>')
    )
  }
  
  p %>% layout(
    polar = list(
      radialaxis = list(visible = TRUE, range = c(0, 100), showline = TRUE,
                        showticklabels = TRUE, gridcolor = "#E8EDF2"),
      angularaxis = list(showline = TRUE, gridcolor = "#E8EDF2")
    ),
    showlegend = TRUE,
    legend = list(orientation = "h", yanchor = "bottom", y = -0.2, xanchor = "center", x = 0.5),
    plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
    margin = list(l = 60, r = 60, t = 20, b = 40)
  ) %>% config(displayModeBar = FALSE)
})

# ============================================
# Prompt Overview — Styled Rankings Table
# ============================================

output$dash_query_rankings_table <- renderUI({
  req(dash_query_timeseries())
  
  data <- dash_query_timeseries()
  if (nrow(data) == 0) {
    return(div(
      style = "text-align: center; padding: 40px; color: #a0aec0;",
      icon("comment-dots", class = "fa-3x", style = "margin-bottom: 15px;"),
      h4("No prompt data yet"),
      p("Select a prompt above to see rankings")
    ))
  }
  
  latest <- data %>%
    group_by(brand_name, brand_id, main_brand_flag) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    arrange(desc(airr_score)) %>%
    mutate(rank = row_number())
  
  score_color <- function(val) {
    if (is.na(val)) return("#cbd5e0")
    if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
  }
  
  rows <- lapply(1:nrow(latest), function(i) {
    row <- latest[i, ]
    is_main <- isTRUE(row$main_brand_flag)
    
    rank_style <- if (i == 1) {
      "background: linear-gradient(135deg, #f6d365, #fda085); color: white;"
    } else if (i == 2) {
      "background: linear-gradient(135deg, #c0c0c0, #e0e0e0); color: #555;"
    } else if (i == 3) {
      "background: linear-gradient(135deg, #cd7f32, #e6a566); color: white;"
    } else {
      "background: #edf2f7; color: #718096;"
    }
    
    row_bg <- if (is_main) {
      "background: linear-gradient(90deg, rgba(102,126,234,0.08) 0%, rgba(102,126,234,0.03) 100%); 
       border-left: 4px solid #667eea;"
    } else {
      "border-left: 4px solid transparent;"
    }
    
    airr_val <- round(row$airr_score, 1)
    
    score_cell <- function(val) {
      div(
        style = "flex: 0 0 52px; text-align: center; font-size: 12px;
             font-weight: 600; color: #4a5568;",
        round(val, 0)
      )
    }
    
    div(
      style = paste0(
        "display: flex; align-items: center; padding: 12px 16px; ",
        "border-bottom: 1px solid #f0f0f0; transition: background 0.2s; ", row_bg
      ),
      class = "leaderboard-row",
      
      div(style = paste0(
        "flex: 0 0 36px; height: 36px; border-radius: 50%; display: flex; ",
        "align-items: center; justify-content: center; font-weight: 700; ",
        "font-size: 14px; margin-right: 16px; ", rank_style), i),
      
      div(
        style = "flex: 1; text-align: center; padding: 0 20px;",
        div(
          style = "display: inline-flex; align-items: center; gap: 8px;",
          tags$span(style = paste0("font-weight: ", if (is_main) "700" else "500",
                                   "; font-size: 16px; color: #2d3748;"), row$brand_name),
          if (is_main) tags$span(
            style = "background: #667eea; color: white; font-size: 9px; padding: 2px 8px; 
                     border-radius: 10px; font-weight: 600; letter-spacing: 0.5px;", "YOUR BRAND")
        )
      ),
      
      div(style = "width: 1px; height: 32px; background: #2d3748; opacity: 0.15; margin: 0 8px;"),
      
      div(style = paste0("flex: 0 0 90px; text-align: center; font-size: 24px; font-weight: 800; color: ",
                         score_color(airr_val), ";"), airr_val),
      
      div(style = "width: 1px; height: 32px; background: #2d3748; opacity: 0.15; margin: 0 8px;"),
      
      score_cell(row$presence_score),
      score_cell(row$perception_score),
      score_cell(row$prestige_score),
      score_cell(row$persistence_score)
    )
  })
  
  div(
    style = "border-radius: 10px; overflow: hidden; border: 1px solid #e2e8f0;",
    
    div(
      style = "display: flex; align-items: center; padding: 14px 16px; 
               background: linear-gradient(135deg, #1a202c 0%, #2d3748 100%);",
      div(style = "flex: 0 0 36px; margin-right: 16px;"),
      div(style = "flex: 1; text-align: center;",
          tags$span(style = "color: #a0aec0; font-weight: 600; font-size: 11px; 
                             text-transform: uppercase; letter-spacing: 1.5px;", "Brand")),
      div(style = "width: 1px; height: 20px; background: #4a5568; margin: 0 8px;"),
      div(style = "flex: 0 0 90px; text-align: center;",
          tags$span(style = "color: #a0aec0; font-weight: 700; font-size: 12px; 
                             text-transform: uppercase; letter-spacing: 1px;", "AiRR")),
      div(style = "width: 1px; height: 20px; background: #4a5568; margin: 0 8px;"),
      div(style = "flex: 0 0 80px; text-align: center;",
          tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                             text-transform: uppercase; letter-spacing: 0.8px;", "Presence")),
      div(style = "flex: 0 0 80px; text-align: center;",
          tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                             text-transform: uppercase; letter-spacing: 0.8px;", "Perception")),
      div(style = "flex: 0 0 80px; text-align: center;",
          tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                             text-transform: uppercase; letter-spacing: 0.8px;", "Prestige")),
      div(style = "flex: 0 0 80px; text-align: center;",
          tags$span(style = "color: #a0aec0; font-weight: 500; font-size: 10px; 
                             text-transform: uppercase; letter-spacing: 0.8px;", "Persistence"))
    ),
    
    do.call(tagList, rows)
  )
})

# ============================================
# Navigate to profiles tab from brand/prompt overview
# ============================================

observeEvent(input$manage_profiles_from_brand, {
  updateTabItems(session, "sidebar", "profiles")
})

observeEvent(input$manage_profiles_from_prompt, {
  updateTabItems(session, "sidebar", "profiles")
})

# ============================================
# Compact rankings helper function
# Used by both brand and prompt overview
# ============================================

render_compact_rankings <- function(data, accent_color = "#667eea") {
  
  if (nrow(data) == 0) {
    return(div(
      style = "text-align: center; padding: 30px; color: #a0aec0;",
      icon("chart-bar", class = "fa-2x", style = "margin-bottom: 10px;"),
      p("No data yet")
    ))
  }
  
  # ── Zero-score cleaning ───────────────────────────────────────────────
  # If any P score is 0 for a row, treat all P scores and AiRR as NA
  data <- data %>%
    mutate(
      any_p_zero = (
        (is.na(presence_score)    | presence_score    == 0) |
          (is.na(perception_score)  | perception_score  == 0) |
          (is.na(prestige_score)    | prestige_score    == 0) |
          (is.na(persistence_score) | persistence_score == 0)
      ),
      airr_score        = ifelse(any_p_zero, NA_real_, airr_score),
      presence_score    = ifelse(any_p_zero, NA_real_, presence_score),
      perception_score  = ifelse(any_p_zero, NA_real_, perception_score),
      prestige_score    = ifelse(any_p_zero, NA_real_, prestige_score),
      persistence_score = ifelse(any_p_zero, NA_real_, persistence_score)
    ) %>%
    select(-any_p_zero)
  
  # Show NA for zero scores
  fmt_score <- function(val) {
    if (is.na(val) || is.null(val) || val == 0) return("NA")
    round(val, 0)
  }
  
  fmt_score_airr <- function(val) {
    if (is.na(val) || is.null(val) || val == 0) return("NA")
    round(val, 1)
  }
  
  score_color <- function(val) {
    if (is.na(val) || is.null(val) || val == 0) return("#cbd5e0")
    if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
  }
  
  score_cell <- function(val) {
    div(
      style = "flex: 0 0 52px; text-align: center; font-size: 12px;
               font-weight: 600; color: #4a5568;",
      fmt_score(val)
    )
  }
  
  # Pre-calculate gaps
  n          <- nrow(data)
  gap_above  <- rep(NA_real_, n)
  gap_below  <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    if (i > 1) gap_above[i] <- round(data$airr_score[i - 1] - data$airr_score[i], 1)
    if (i < n) gap_below[i] <- round(data$airr_score[i] - data$airr_score[i + 1], 1)
  }
  
  rows <- lapply(1:nrow(data), function(i) {
    row     <- data[i, ]
    is_main <- isTRUE(row$main_brand_flag)
    airr_val <- round(row$airr_score, 1)
    
    rank_style <- if (i == 1) {
      "background: linear-gradient(135deg, #f6d365, #fda085); color: white;"
    } else if (i == 2) {
      "background: linear-gradient(135deg, #c0c0c0, #e0e0e0); color: #555;"
    } else if (i == 3) {
      "background: linear-gradient(135deg, #cd7f32, #e6a566); color: white;"
    } else {
      "background: #edf2f7; color: #718096;"
    }
    
    row_bg <- if (is_main) {
      paste0(
        "background: linear-gradient(90deg, ",
        "rgba(142,68,173,0.08) 0%, rgba(142,68,173,0.03) 100%); ",
        "border-left: 3px solid ", accent_color, ";"
      )
    } else {
      "border-left: 3px solid transparent;"
    }
    
    tooltip_lines <- c()
    if (!is.na(gap_above[i])) {
      tooltip_lines <- c(tooltip_lines,
                         paste0("\u25b2 ", gap_above[i], " pts behind ", data$brand_name[i - 1]))
    }
    if (!is.na(gap_below[i])) {
      tooltip_lines <- c(tooltip_lines,
                         paste0("\u25bc ", gap_below[i], " pts ahead of ", data$brand_name[i + 1]))
    }
    if (length(tooltip_lines) == 0) tooltip_lines <- "Only brand tracked"
    tooltip_text <- paste(tooltip_lines, collapse = "\n")
    
    div(
      style = paste0(
        "display: flex; align-items: center; padding: 7px 10px; ",
        "border-bottom: 1px solid #f0f0f0; gap: 6px; ",
        "position: relative; cursor: default; ",
        row_bg
      ),
      class = "leaderboard-row ldb-tooltip-anchor",
      
      # Rank badge
      div(
        style = paste0(
          "flex: 0 0 22px; height: 22px; border-radius: 50%; display: flex; ",
          "align-items: center; justify-content: center; font-weight: 700; ",
          "font-size: 10px; flex-shrink: 0; ", rank_style
        ),
        i
      ),
      
      # Brand name + YOU badge
      div(
        style = "flex: 1; min-width: 0; display: flex; align-items: center; gap: 4px;",
        tags$span(
          style = paste0(
            "font-size: 12px; font-weight: ", if (is_main) "700" else "500",
            "; color: #2d3748; white-space: nowrap; overflow: hidden; ",
            "text-overflow: ellipsis;"
          ),
          row$brand_name
        ),
        if (is_main) tags$span(
          style = paste0(
            "background: ", accent_color, "; color: white; font-size: 7px; ",
            "padding: 1px 4px; border-radius: 5px; font-weight: 600; ",
            "white-space: nowrap; flex-shrink: 0; line-height: 1.6;"
          ),
          "YOU"
        )
      ),
      
      # AiRR score
      div(
        style = paste0(
          "flex: 0 0 40px; text-align: right; font-size: 16px; font-weight: 800; ",
          "color: ", score_color(airr_val), "; flex-shrink: 0;"
        ),
        fmt_score_airr(airr_val)
      ),
      
      div(style = "flex: 0 0 1px; height: 24px; background: #e2e8f0; flex-shrink: 0;"),
      
      score_cell(row$presence_score),
      score_cell(row$perception_score),
      score_cell(row$prestige_score),
      score_cell(row$persistence_score),
      
      # Tooltip
      tags$div(
        class = "ldb-tooltip",
        style = paste0(
          "position: absolute; right: 8px; top: 50%; transform: translateY(-50%); ",
          "background: #1a202c; color: white; font-size: 11px; line-height: 1.7; ",
          "padding: 8px 12px; border-radius: 8px; white-space: pre; ",
          "pointer-events: none; opacity: 0; transition: opacity 0.15s ease; ",
          "z-index: 9999; box-shadow: 0 4px 12px rgba(0,0,0,0.25);"
        ),
        tooltip_text
      )
    )
  })
  
  # ── Header ──────────────────────────────────────────────────────────────
  tagList(
    
    # Leaderboard heading
    div(
      style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
      icon("trophy", style = "color: #D4A843; font-size: 13px;"),
      tags$span(
        style = "font-size: 12px; font-weight: 700; text-transform: uppercase;
                 letter-spacing: 1px; color: #4a5568;",
        "Leaderboard"
      )
    ),
    
    div(
      div(
        style = "display: flex; align-items: center; padding: 8px 10px;
                 background: linear-gradient(135deg, #1a202c 0%, #2d3748 100%);
                 border-radius: 8px 8px 0 0; gap: 6px;",
        
        div(style = "flex: 0 0 22px; flex-shrink: 0;"),
        
        div(
          style = "flex: 1;",
          tags$span(
            style = "color: #a0aec0; font-size: 9px; font-weight: 600;
                     text-transform: uppercase; letter-spacing: 1px;",
            "Brand"
          )
        ),
        
        div(
          style = "flex: 0 0 40px; text-align: right; flex-shrink: 0;",
          tags$span(
            style = "color: #a0aec0; font-size: 9px; font-weight: 700;
                     text-transform: uppercase; letter-spacing: 1px;",
            "AiRR"
          )
        ),
        
        div(style = "flex: 0 0 1px; height: 20px; background: #4a5568; flex-shrink: 0;"),
        
        div(
          class = "ldb-col-header",
          style = "flex: 0 0 52px; border-bottom-color: #27AE60;",
          `data-tooltip` = "Presence",
          tags$span(style = "color: #27AE60;", "Pres")
        ),
        div(
          class = "ldb-col-header",
          style = "flex: 0 0 52px; border-bottom-color: #D4A843;",
          `data-tooltip` = "Perception",
          tags$span(style = "color: #D4A843;", "Perc")
        ),
        div(
          class = "ldb-col-header",
          style = "flex: 0 0 52px; border-bottom-color: #8E44AD;",
          `data-tooltip` = "Prestige",
          tags$span(style = "color: #8E44AD;", "Prest")
        ),
        div(
          class = "ldb-col-header",
          style = "flex: 0 0 52px; border-bottom-color: #2980B9;",
          `data-tooltip` = "Persistence",
          tags$span(style = "color: #2980B9;", "Pers")
        )
      ),
      
      div(
        style = "border: 1px solid #e2e8f0; border-top: none;
                 border-radius: 0 0 8px 8px; overflow: hidden;",
        do.call(tagList, rows)
      )
    )
  )
}

# ============================================
# Brand overview compact rankings
# ============================================

output$dash_rankings_table_compact <- renderUI({
  req(dash_latest_scores())
  
  data <- dash_latest_scores() %>%
    arrange(desc(airr_score)) %>%
    mutate(rank = row_number())
  
  render_compact_rankings(data)
})

# ============================================
# Prompt overview compact rankings
# ============================================

output$dash_query_rankings_table_compact <- renderUI({
  req(dash_query_timeseries())
  
  data <- dash_query_timeseries()
  if (nrow(data) == 0) return(NULL)
  
  data <- data %>%
    group_by(brand_name, brand_id, main_brand_flag) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    arrange(desc(airr_score)) %>%
    mutate(rank = row_number())
  
  render_compact_rankings(data)
})

# ============================================
# Brand Overview — Customer Profiles section
# ============================================

output$brand_overview_profiles_section <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  sub <- user_subscription()
  
  # Non-Enterprise gate
  if (sub$subscription_name != "Enterprise") {
    return(
      div(
        style = "text-align: center; padding: 30px;",
        div(
          style = "background: linear-gradient(135deg, rgba(142,68,173,0.06), 
                   rgba(142,68,173,0.02)); border: 1px dashed rgba(142,68,173,0.3);
                   border-radius: 12px; padding: 30px 20px; max-width: 500px; 
                   margin: 0 auto;",
          icon("crown", class = "fa-2x", style = "color: #8E44AD; margin-bottom: 12px;"),
          div(style = "font-size: 15px; font-weight: 600; color: #2d3748; margin-bottom: 6px;",
              "Customer persona — Enterprise Feature"),
          div(style = "font-size: 13px; color: #718096; margin-bottom: 16px; line-height: 1.5;",
              "See how different customer segments perceive your brand vs competitors."),
          actionButton("upgrade_from_brand_profiles", "View Plans",
                       icon = icon("arrow-up"),
                       style = "background: #8E44AD; color: white; border: none;
                                border-radius: 8px; padding: 8px 20px; font-weight: 600;")
        )
      )
    )
  }
  
  profiles <- user_profiles()
  
  if (nrow(profiles) == 0) {
    return(
      div(
        style = "text-align: center; padding: 30px; color: #a0aec0;",
        icon("users", class = "fa-2x", style = "margin-bottom: 10px; color: #e2e8f0;"),
        p("No personas set up yet."),
        actionButton("manage_profiles_from_brand2", "Add personas",
                     icon = icon("plus"),
                     style = "background: #8E44AD; color: white; border: none;
                              border-radius: 8px; padding: 8px 20px; font-weight: 600;")
      )
    )
  }
  
  # Profile score cards grid
  div(
    style = "display: flex; flex-wrap: wrap; gap: 16px;",
    lapply(1:nrow(profiles), function(i) {
      pid   <- profiles$profile_id[i]
      pname <- profiles$profile_name[i]
      uiOutput(paste0("brand_overview_profile_card_", pid))
    })
  )
})

# Render individual profile cards for brand overview
observe({
  req(rv$logged_in, rv$login_id)
  profiles <- tryCatch(user_profiles(), error = function(e) NULL)
  if (is.null(profiles) || nrow(profiles) == 0) return()
  
  lapply(1:nrow(profiles), function(i) {
    pid        <- profiles$profile_id[i]
    pname      <- profiles$profile_name[i]
    local_pid  <- pid
    local_name <- pname
    
    output[[paste0("brand_overview_profile_card_", local_pid)]] <- renderUI({
      scores <- get_profile_brand_scores(rv$login_id, local_pid)
      
      score_color <- function(val) {
        if (is.na(val) || is.null(val)) return("#cbd5e0")
        if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
      }
      
      div(
        style = "background: white; border-radius: 12px; padding: 16px;
                 border: 1px solid #e2e8f0; min-width: 220px; flex: 1;
                 border-top: 3px solid #8E44AD;",
        
        # Profile name header
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 14px;",
          div(
            style = "width: 28px; height: 28px; border-radius: 8px; 
                     background: rgba(142,68,173,0.1); display: flex; 
                     align-items: center; justify-content: center; color: #8E44AD;",
            icon("users", style = "font-size: 12px;")
          ),
          div(
            style = "font-size: 13px; font-weight: 600; color: #2d3748; 
                     white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
            local_name
          )
        ),
        
        if (is.null(scores) || nrow(scores) == 0) {
          div(
            style = "text-align: center; padding: 15px 0; color: #a0aec0;",
            tags$i(class = "fa fa-spinner fa-spin",
                   style = "font-size: 16px; color: #8E44AD;"),
            p(style = "font-size: 11px; margin-top: 6px;", "Calculating...")
          )
        } else {
          # Show main brand score prominently, then competitors below
          main  <- scores %>% filter(main_brand_flag == TRUE)
          comps <- scores %>% filter(main_brand_flag == FALSE) %>%
            arrange(desc(airr_score))
          
          tagList(
            # Main brand
            if (nrow(main) > 0) {
              div(
                style = "margin-bottom: 10px;",
                div(
                  style = "display: flex; justify-content: space-between; 
                           align-items: center; margin-bottom: 4px;",
                  tags$span(
                    style = "font-size: 12px; font-weight: 700; color: #2d3748;",
                    paste0("★ ", main$brand_name[1])
                  ),
                  tags$span(
                    style = paste0("font-size: 18px; font-weight: 800; color: ",
                                   score_color(main$airr_score[1]), ";"),
                    round(main$airr_score[1], 1)
                  )
                ),
                # Mini sub-score bar
                div(
                  style = "display: flex; gap: 4px;",
                  lapply(list(
                    list(v = main$presence_score[1],   l = "Pres"),
                    list(v = main$perception_score[1], l = "Perc"),
                    list(v = main$prestige_score[1],   l = "Prest"),
                    list(v = main$persistence_score[1],l = "Pers")
                  ), function(s) {
                    div(
                      style = "flex: 1; text-align: center;",
                      div(style = paste0("font-size: 11px; font-weight: 700; color: ",
                                         score_color(s$v), ";"),
                          round(s$v, 0)),
                      div(style = "font-size: 9px; color: #a0aec0;", s$l)
                    )
                  })
                )
              )
            },
            
            # Divider
            if (nrow(comps) > 0) hr(style = "margin: 8px 0; border-color: #f0f0f0;"),
            
            # Competitors
            if (nrow(comps) > 0) {
              div(
                lapply(1:nrow(comps), function(j) {
                  div(
                    style = "display: flex; justify-content: space-between; 
                             align-items: center; padding: 3px 0;",
                    tags$span(
                      style = "font-size: 12px; color: #4a5568; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; max-width: 130px;",
                      comps$brand_name[j]
                    ),
                    tags$span(
                      style = paste0("font-size: 14px; font-weight: 700; color: ",
                                     score_color(comps$airr_score[j]), ";"),
                      round(comps$airr_score[j], 1)
                    )
                  )
                })
              )
            }
          )
        }
      )
    })
  })
})

observeEvent(input$upgrade_from_brand_profiles, {
  shinyjs::click("upgrade_btn")
})

observeEvent(input$manage_profiles_from_brand2, {
  updateTabItems(session, "sidebar", "profiles")
})

# ============================================
# Selected profile reactives
# ============================================

selected_brand_profile_id   <- reactiveVal(NULL)
selected_prompt_profile_id  <- reactiveVal(NULL)

observeEvent(input$brand_profile_card_click, {
  pid <- input$brand_profile_card_click
  # Toggle off if clicking same card again
  if (!is.null(selected_brand_profile_id()) && 
      selected_brand_profile_id() == pid) {
    selected_brand_profile_id(NULL)
  } else {
    selected_brand_profile_id(pid)
  }
})

observeEvent(input$prompt_profile_card_click, {
  pid <- input$prompt_profile_card_click
  if (!is.null(selected_prompt_profile_id()) && 
      selected_prompt_profile_id() == pid) {
    selected_prompt_profile_id(NULL)
  } else {
    selected_prompt_profile_id(pid)
  }
})

# ============================================
# Brand profile detail panel
# ============================================

output$brand_profile_detail_panel <- renderUI({
  pid <- selected_brand_profile_id()
  if (is.null(pid)) return(NULL)
  
  profiles <- user_profiles()
  prof_row <- profiles[profiles$profile_id == pid, ]
  if (nrow(prof_row) == 0) return(NULL)
  
  pname <- prof_row$profile_name[1]
  
  div(
    style = "margin-top: 20px; border-top: 2px solid rgba(142,68,173,0.2); 
             padding-top: 20px;",
    
    # Detail header
    div(
      style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
      div(
        style = "width: 36px; height: 36px; border-radius: 10px; 
                 background: rgba(142,68,173,0.15); display: flex; 
                 align-items: center; justify-content: center; color: #8E44AD;",
        icon("users")
      ),
      div(
        div(style = "font-size: 16px; font-weight: 700; color: #2d3748;", pname),
        div(style = "font-size: 12px; color: #a0aec0;", 
            "Click the card again to close")
      ),
      div(
        style = "margin-left: auto;",
        tags$button(
          style = "background: none; border: 1px solid #e2e8f0; border-radius: 8px;
                   padding: 4px 12px; color: #718096; cursor: pointer; font-size: 12px;",
          onclick = "Shiny.setInputValue('brand_profile_card_click', null, 
                     {priority: 'event'})",
          icon("times"), " Close"
        )
      )
    ),
    
    # Line charts + rankings side by side
    div(
      style = "display: flex; gap: 20px; align-items: stretch; margin-bottom: 20px;",
      
      # Trend charts
      div(
        style = "flex: 1; min-width: 0;",
        tabsetPanel(
          id = paste0("brand_profile_chart_tabs_", pid),
          type = "tabs",
          tabPanel("AIRR Score",
                   withSpinner(
                     plotlyOutput(paste0("bp_chart_airr_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Presence",
                   withSpinner(
                     plotlyOutput(paste0("bp_chart_presence_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Perception",
                   withSpinner(
                     plotlyOutput(paste0("bp_chart_perception_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Prestige",
                   withSpinner(
                     plotlyOutput(paste0("bp_chart_prestige_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Persistence",
                   withSpinner(
                     plotlyOutput(paste0("bp_chart_persistence_", pid), height = "380px"),
                     type = 4, color = "#8E44AD"))
        )
      ),
      
      # Compact rankings — vertically centred
      div(
        style = "flex: 0 0 360px; min-width: 300px; display: flex; 
                 align-items: center;",
        div(
          style = "width: 100%;",
          withSpinner(
            uiOutput(paste0("bp_rankings_", pid)),
            type = 4, color = "#8E44AD"
          )
        )
      )
    ),
    
    # Quadrant charts side by side (replaces spider)
    div(
      style = "display: flex; gap: 20px;",
      
      div(
        style = "flex: 1; min-width: 0; background: rgba(142,68,173,0.03); 
                 border-radius: 10px; padding: 12px; 
                 border: 1px solid rgba(142,68,173,0.1);",
        withSpinner(
          plotlyOutput(paste0("bp_quadrant_positioning_", pid), height = "340px"),
          type = 4, color = "#8E44AD"
        )
      ),
      
      div(
        style = "flex: 1; min-width: 0; background: rgba(142,68,173,0.03); 
                 border-radius: 10px; padding: 12px; 
                 border: 1px solid rgba(142,68,173,0.1);",
        withSpinner(
          plotlyOutput(paste0("bp_quadrant_momentum_", pid), height = "340px"),
          type = 4, color = "#8E44AD"
        )
      )
    )
  )
})

# ============================================
# Render brand profile detail charts + rankings
# whenever selected_brand_profile_id changes
# ============================================

observe({
  pid <- selected_brand_profile_id()
  if (is.null(pid)) return()
  
  req(rv$logged_in, rv$login_id)
  local_pid <- pid
  
  ts_data <- get_profile_brand_timeseries(rv$login_id, local_pid)
  
  chart_specs <- list(
    list(id = "airr",        col = "airr_score",        label = "AIRR Score"),
    list(id = "presence",    col = "presence_score",    label = "Presence Score"),
    list(id = "perception",  col = "perception_score",  label = "Perception Score"),
    list(id = "prestige",    col = "prestige_score",    label = "Prestige Score"),
    list(id = "persistence", col = "persistence_score", label = "Persistence Score")
  )
  
  # Line charts
  lapply(chart_specs, function(spec) {
    output[[paste0("bp_chart_", spec$id, "_", local_pid)]] <- renderPlotly({
      if (is.null(ts_data) || nrow(ts_data) == 0) {
        return(plotly_empty_state("No data available yet"))
      }
      create_profile_dash_chart(ts_data, spec$col, spec$label, rv$brand_name)
    })
  })
  
  # Rankings
  output[[paste0("bp_rankings_", local_pid)]] <- renderUI({
    scores <- get_profile_brand_scores(rv$login_id, local_pid)
    if (is.null(scores) || nrow(scores) == 0) {
      return(div(
        style = "text-align: center; padding: 30px; color: #a0aec0;",
        tags$i(class = "fa fa-spinner fa-spin",
               style = "font-size: 20px; color: #8E44AD;"),
        p(style = "margin-top: 10px; font-size: 13px;", "Calculating scores...")
      ))
    }
    data <- scores %>%
      arrange(desc(airr_score)) %>%
      mutate(rank = row_number())
    render_compact_rankings(data, accent_color = "#8E44AD")
  })
  
  # Quadrant charts
  output[[paste0("bp_quadrant_positioning_", local_pid)]] <- renderPlotly({
    scores <- get_profile_brand_scores(rv$login_id, local_pid)
    if (is.null(scores) || nrow(scores) == 0 || nrow(scores) < 2) return(NULL)
    create_quadrant_chart(
      data            = scores,
      x_col           = "presence_score",
      y_col           = "perception_score",
      x_label         = "Presence",
      y_label         = "Perception",
      title           = "Positioning — Presence vs Perception",
      bubble_col      = "prestige_score",
      bubble_label    = "Prestige",
      main_brand_name = rv$brand_name
    )
  })
  
  output[[paste0("bp_quadrant_momentum_", local_pid)]] <- renderPlotly({
    scores <- get_profile_brand_scores(rv$login_id, local_pid)
    if (is.null(scores) || nrow(scores) == 0 || nrow(scores) < 2) return(NULL)
    create_quadrant_chart(
      data            = scores,
      x_col           = "persistence_score",
      y_col           = "prestige_score",
      x_label         = "Persistence (Stability)",
      y_label         = "Prestige (Authority)",
      title           = "Momentum — Stability vs Authority",
      bubble_col      = "airr_score",
      bubble_label    = "AiRR",
      main_brand_name = rv$brand_name
    )
  })
})

# ============================================
# Prompt profile detail panel
# ============================================

output$prompt_profile_detail_panel <- renderUI({
  pid <- selected_prompt_profile_id()
  if (is.null(pid)) return(NULL)
  req(input$dash_query_select, input$dash_query_select != "")
  
  profiles <- user_profiles()
  prof_row <- profiles[profiles$profile_id == pid, ]
  if (nrow(prof_row) == 0) return(NULL)
  
  pname        <- prof_row$profile_name[1]
  query_string <- input$dash_query_select
  
  div(
    style = "margin-top: 20px; border-top: 2px solid rgba(142,68,173,0.2); 
             padding-top: 20px;",
    
    # Detail header
    div(
      style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
      div(
        style = "width: 36px; height: 36px; border-radius: 10px; 
                 background: rgba(142,68,173,0.15); display: flex; 
                 align-items: center; justify-content: center; color: #8E44AD;",
        icon("users")
      ),
      div(
        div(style = "font-size: 16px; font-weight: 700; color: #2d3748;", pname),
        div(style = "font-size: 12px; color: #a0aec0;",
            paste0("Prompt: ", substr(query_string, 1, 60),
                   if (nchar(query_string) > 60) "..." else ""))
      ),
      div(
        style = "margin-left: auto;",
        tags$button(
          style = "background: none; border: 1px solid #e2e8f0; border-radius: 8px;
                   padding: 4px 12px; color: #718096; cursor: pointer; font-size: 12px;",
          onclick = "Shiny.setInputValue('prompt_profile_card_click', null, 
                     {priority: 'event'})",
          icon("times"), " Close"
        )
      )
    ),
    
    # Line charts + rankings side by side
    div(
      style = "display: flex; gap: 20px; align-items: stretch; margin-bottom: 20px;",
      
      div(
        style = "flex: 1; min-width: 0;",
        tabsetPanel(
          id = paste0("prompt_profile_chart_tabs_", pid),
          type = "tabs",
          tabPanel("AIRR Score",
                   withSpinner(
                     plotlyOutput(paste0("pp_chart_airr_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Presence",
                   withSpinner(
                     plotlyOutput(paste0("pp_chart_presence_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Perception",
                   withSpinner(
                     plotlyOutput(paste0("pp_chart_perception_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Prestige",
                   withSpinner(
                     plotlyOutput(paste0("pp_chart_prestige_", pid), height = "380px"),
                     type = 4, color = "#8E44AD")),
          tabPanel("Persistence",
                   withSpinner(
                     plotlyOutput(paste0("pp_chart_persistence_", pid), height = "380px"),
                     type = 4, color = "#8E44AD"))
        )
      ),
      
      # Compact rankings — vertically centred
      div(
        style = "flex: 0 0 360px; min-width: 300px; display: flex; 
                 align-items: center;",
        div(
          style = "width: 100%;",
          withSpinner(
            uiOutput(paste0("pp_rankings_", pid)),
            type = 4, color = "#8E44AD"
          )
        )
      )
    ),
    
    # Quadrant charts side by side (replaces spider)
    div(
      style = "display: flex; gap: 20px;",
      
      div(
        style = "flex: 1; min-width: 0; background: rgba(142,68,173,0.03); 
                 border-radius: 10px; padding: 12px; 
                 border: 1px solid rgba(142,68,173,0.1);",
        withSpinner(
          plotlyOutput(paste0("pp_quadrant_positioning_", pid), height = "340px"),
          type = 4, color = "#8E44AD"
        )
      ),
      
      div(
        style = "flex: 1; min-width: 0; background: rgba(142,68,173,0.03); 
                 border-radius: 10px; padding: 12px; 
                 border: 1px solid rgba(142,68,173,0.1);",
        withSpinner(
          plotlyOutput(paste0("pp_quadrant_momentum_", pid), height = "340px"),
          type = 4, color = "#8E44AD"
        )
      )
    )
  )
})

# ============================================
# Render prompt profile detail charts + rankings
# whenever selected_prompt_profile_id or query changes
# ============================================

observe({
  pid <- selected_prompt_profile_id()
  if (is.null(pid)) return()
  req(rv$logged_in, rv$login_id)
  req(input$dash_query_select, input$dash_query_select != "")
  
  local_pid    <- pid
  query_string <- input$dash_query_select
  
  ts_data <- get_profile_query_timeseries(rv$login_id, local_pid, query_string)
  
  chart_specs <- list(
    list(id = "airr",        col = "airr_score",        label = "AIRR Score"),
    list(id = "presence",    col = "presence_score",    label = "Presence Score"),
    list(id = "perception",  col = "perception_score",  label = "Perception Score"),
    list(id = "prestige",    col = "prestige_score",    label = "Prestige Score"),
    list(id = "persistence", col = "persistence_score", label = "Persistence Score")
  )
  
  lapply(chart_specs, function(spec) {
    output[[paste0("pp_chart_", spec$id, "_", local_pid)]] <- renderPlotly({
      if (is.null(ts_data) || nrow(ts_data) == 0) {
        return(plotly_empty_state("No data available yet"))
      }
      create_profile_dash_chart(ts_data, spec$col, spec$label, rv$brand_name)
    })
  })
  
  output[[paste0("pp_rankings_", local_pid)]] <- renderUI({
    scores <- get_profile_query_scores(rv$login_id, local_pid, query_string)
    if (is.null(scores) || nrow(scores) == 0) {
      return(div(
        style = "text-align: center; padding: 30px; color: #a0aec0;",
        tags$i(class = "fa fa-spinner fa-spin",
               style = "font-size: 20px; color: #8E44AD;"),
        p(style = "margin-top: 10px; font-size: 13px;", "Calculating scores...")
      ))
    }
    data <- scores %>%
      arrange(desc(airr_score)) %>%
      mutate(rank = row_number())
    render_compact_rankings(data, accent_color = "#8E44AD")
  })
  
  # Quadrant charts
  output[[paste0("pp_quadrant_positioning_", local_pid)]] <- renderPlotly({
    scores <- get_profile_query_scores(rv$login_id, local_pid, query_string)
    if (is.null(scores) || nrow(scores) == 0 || nrow(scores) < 2) return(NULL)
    create_quadrant_chart(
      data            = scores,
      x_col           = "presence_score",
      y_col           = "perception_score",
      x_label         = "Presence",
      y_label         = "Perception",
      title           = "Positioning — Presence vs Perception",
      bubble_col      = "prestige_score",
      bubble_label    = "Prestige",
      main_brand_name = rv$brand_name
    )
  })
  
  output[[paste0("pp_quadrant_momentum_", local_pid)]] <- renderPlotly({
    scores <- get_profile_query_scores(rv$login_id, local_pid, query_string)
    if (is.null(scores) || nrow(scores) == 0 || nrow(scores) < 2) return(NULL)
    create_quadrant_chart(
      data            = scores,
      x_col           = "persistence_score",
      y_col           = "prestige_score",
      x_label         = "Persistence (Stability)",
      y_label         = "Prestige (Authority)",
      title           = "Momentum — Stability vs Authority",
      bubble_col      = "airr_score",
      bubble_label    = "AiRR",
      main_brand_name = rv$brand_name
    )
  })
})

create_quadrant_chart <- function(data, 
                                  x_col, y_col, 
                                  x_label, y_label,
                                  title,
                                  bubble_col = NULL,
                                  bubble_label = NULL,
                                  main_brand_name = NULL) {
  
  req(nrow(data) >= 2)
  
  x_mid <- 50
  y_mid <- 50
  
  # Bubble size
  if (!is.null(bubble_col) && bubble_col %in% names(data)) {
    marker_size <- scales::rescale(data[[bubble_col]], to = c(15, 45))
  } else {
    marker_size <- rep(20, nrow(data))
  }
  
  # Colour: highlight main brand in gold
  comp_colours <- c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6",
                    "#E67E22", "#1ABC9C", "#34495E", "#F39C12",
                    "#16A085", "#C0392B")
  
  colours <- sapply(seq_len(nrow(data)), function(i) {
    if (!is.null(main_brand_name) && 
        !is.na(data$brand_name[i]) && 
        data$brand_name[i] == main_brand_name) {
      "#D4A843"
    } else {
      comp_colours[((i - 1) %% length(comp_colours)) + 1]
    }
  })
  
  p <- plot_ly()
  
  # Add quadrant shading
  p <- p %>%
    add_trace(
      type = "scatter", mode = "markers",
      x = c(0, 50, 50, 0, 0),
      y = c(50, 50, 100, 100, 50),
      fill = "toself",
      fillcolor = "rgba(52, 152, 219, 0.04)",
      line = list(color = "transparent"),
      showlegend = FALSE, hoverinfo = "none"
    ) %>%
    add_trace(
      type = "scatter", mode = "markers",
      x = c(50, 100, 100, 50, 50),
      y = c(50, 50, 100, 100, 50),
      fill = "toself",
      fillcolor = "rgba(39, 174, 96, 0.04)",
      line = list(color = "transparent"),
      showlegend = FALSE, hoverinfo = "none"
    ) %>%
    add_trace(
      type = "scatter", mode = "markers",
      x = c(0, 50, 50, 0, 0),
      y = c(0, 0, 50, 50, 0),
      fill = "toself",
      fillcolor = "rgba(189, 195, 199, 0.04)",
      line = list(color = "transparent"),
      showlegend = FALSE, hoverinfo = "none"
    ) %>%
    add_trace(
      type = "scatter", mode = "markers",
      x = c(50, 100, 100, 50, 50),
      y = c(0, 0, 50, 50, 0),
      fill = "toself",
      fillcolor = "rgba(230, 126, 34, 0.04)",
      line = list(color = "transparent"),
      showlegend = FALSE, hoverinfo = "none"
    )
  
  # Add dividing lines
  p <- p %>%
    add_segments(
      x = x_mid, xend = x_mid, y = 0, yend = 100,
      line = list(color = "rgba(0,0,0,0.12)", width = 1.5, dash = "dot"),
      showlegend = FALSE, hoverinfo = "none"
    ) %>%
    add_segments(
      x = 0, xend = 100, y = y_mid, yend = y_mid,
      line = list(color = "rgba(0,0,0,0.12)", width = 1.5, dash = "dot"),
      showlegend = FALSE, hoverinfo = "none"
    )
  
  # Add brand bubbles
  # Add competitor bubbles — no text labels, hover only
  comp_data <- data[sapply(seq_len(nrow(data)), function(i) {
    is.null(main_brand_name) || is.na(data$brand_name[i]) || 
      data$brand_name[i] != main_brand_name
  }), ]
  
  if (nrow(comp_data) > 0) {
    for (i in seq_len(nrow(comp_data))) {
      
      hover_text <- paste0(
        "<b>", comp_data$brand_name[i], "</b><br>",
        x_label, ": ", round(comp_data[[x_col]][i], 1), "<br>",
        y_label, ": ", round(comp_data[[y_col]][i], 1),
        if (!is.null(bubble_col)) {
          paste0("<br>", if (!is.null(bubble_label)) bubble_label else bubble_col,
                 ": ", round(comp_data[[bubble_col]][i], 1))
        } else ""
      )
      
      p <- p %>% add_trace(
        type      = "scatter",
        mode      = "markers",          # no text
        x         = comp_data[[x_col]][i],
        y         = comp_data[[y_col]][i],
        marker    = list(
          size    = marker_size[which(data$brand_name == comp_data$brand_name[i])[1]],
          color   = colours[which(data$brand_name == comp_data$brand_name[i])[1]],
          opacity = 0.8,
          line    = list(color = "white", width = 1.5)
        ),
        name      = comp_data$brand_name[i],
        hovertext = hover_text,
        hoverinfo = "text",
        showlegend = FALSE
      )
    }
  }
  
  # Add main brand bubble — with text label
  main_data <- data[!is.null(main_brand_name) & 
                      !is.na(data$brand_name) & 
                      data$brand_name == main_brand_name, ]
  
  if (nrow(main_data) > 0) {
    main_idx <- which(data$brand_name == main_brand_name)[1]
    
    hover_text <- paste0(
      "<b>", main_brand_name, "</b><br>",
      x_label, ": ", round(main_data[[x_col]][1], 1), "<br>",
      y_label, ": ", round(main_data[[y_col]][1], 1),
      if (!is.null(bubble_col)) {
        paste0("<br>", if (!is.null(bubble_label)) bubble_label else bubble_col,
               ": ", round(main_data[[bubble_col]][1], 1))
      } else ""
    )
    
    p <- p %>% add_trace(
      type         = "scatter",
      mode         = "markers+text",    # label only for main brand
      x            = main_data[[x_col]][1],
      y            = main_data[[y_col]][1],
      marker       = list(
        size       = marker_size[main_idx],
        color      = "#D4A843",
        opacity    = 1.0,
        line       = list(color = "#1A1A1A", width = 2.5)
      ),
      text         = paste0("★ ", main_brand_name),
      textposition = "top center",
      textfont     = list(
        size   = 13,
        color  = "#D4A843",
        family = "Inter"
      ),
      name         = main_brand_name,
      hovertext    = hover_text,
      hoverinfo    = "text",
      showlegend   = FALSE
    )
  }
  
  p %>% layout(
    title = list(
      text = title,
      font = list(size = 14, color = "#2d3748", family = "Inter"),
      x = 0.05
    ),
    xaxis = list(
      title = list(text = x_label, 
                   font = list(size = 12, color = "#718096", family = "Inter")),
      range = c(0, 100),
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.05)",
      zeroline = FALSE,
      tickfont = list(size = 10, color = "#9E9E9E", family = "Inter")
    ),
    yaxis = list(
      title = list(text = y_label,
                   font = list(size = 12, color = "#718096", family = "Inter")),
      range = c(0, 100),
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.05)",
      zeroline = FALSE,
      tickfont = list(size = 10, color = "#9E9E9E", family = "Inter")
    ),
    plot_bgcolor  = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 60, r = 20, t = 50, b = 60),
    font   = list(family = "Inter")
  ) %>%
    config(displayModeBar = FALSE)
}

# Positioning quadrant (Presence vs Perception)
output$dash_quadrant_positioning <- renderPlotly({
  req(dash_latest_scores())
  data <- dash_latest_scores()
  if (nrow(data) < 2) return(NULL)
  
  create_quadrant_chart(
    data            = data,
    x_col           = "presence_score",
    y_col           = "perception_score",
    x_label         = "Presence",
    y_label         = "Perception",
    title           = "AI Positioning — Presence vs Perception",
    bubble_col      = "prestige_score",
    bubble_label    = "Prestige",
    main_brand_name = rv$brand_name
  )
})

# Momentum quadrant (Persistence vs Prestige)
output$dash_quadrant_momentum <- renderPlotly({
  req(dash_latest_scores())
  data <- dash_latest_scores()
  if (nrow(data) < 2) return(NULL)
  
  create_quadrant_chart(
    data            = data,
    x_col           = "persistence_score",
    y_col           = "prestige_score",
    x_label         = "Persistence (Stability)",
    y_label         = "Prestige (Authority)",
    title           = "AI Momentum — Stability vs Authority",
    bubble_col      = "airr_score",
    bubble_label    = "AiRR",
    main_brand_name = rv$brand_name
  )
})

# Prompt positioning quadrant
output$dash_query_quadrant_positioning <- renderPlotly({
  req(dash_query_timeseries())
  
  data <- dash_query_timeseries()
  if (nrow(data) == 0) return(NULL)
  
  latest <- data %>%
    group_by(brand_name, brand_id, main_brand_flag) %>%
    filter(date == max(date)) %>%
    ungroup()
  
  if (nrow(latest) < 2) return(NULL)
  
  create_quadrant_chart(
    data            = latest,
    x_col           = "presence_score",
    y_col           = "perception_score",
    x_label         = "Presence",
    y_label         = "Perception",
    title           = "Positioning — Presence vs Perception",
    bubble_col      = "prestige_score",
    bubble_label    = "Prestige",
    main_brand_name = rv$brand_name
  )
})

# Prompt momentum quadrant
output$dash_query_quadrant_momentum <- renderPlotly({
  req(dash_query_timeseries())
  
  data <- dash_query_timeseries()
  if (nrow(data) == 0) return(NULL)
  
  latest <- data %>%
    group_by(brand_name, brand_id, main_brand_flag) %>%
    filter(date == max(date)) %>%
    ungroup()
  
  if (nrow(latest) < 2) return(NULL)
  
  create_quadrant_chart(
    data            = latest,
    x_col           = "persistence_score",
    y_col           = "prestige_score",
    x_label         = "Persistence (Stability)",
    y_label         = "Prestige (Authority)",
    title           = "Momentum — Stability vs Authority",
    bubble_col      = "airr_score",
    bubble_label    = "AiRR",
    main_brand_name = rv$brand_name
  )
})

# Navigate to account tab from brand overview industry nudge
observeEvent(input$nav_to_account, {
  updateTabItems(session, "sidebar", "account")
})

output$sidebar_brand_card <- renderUI({
  req(rv$logged_in, rv$brand_name)
  
  info <- tryCatch(
    dbGetQuery(pool, "
      SELECT b.brand_reach, b.reach_country, b.reach_region,
             b.reach_postcode, ubt.industry
      FROM fact_user_brands_tracked ubt
      JOIN dim_brand b ON b.brand_id = ubt.brand_id
      WHERE ubt.login_id = $1
        AND ubt.main_brand_flag = TRUE
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
      LIMIT 1",
               params = list(rv$login_id)),
    error = function(e) NULL
  )
  
  industry_str <- if (!is.null(info) && nrow(info) > 0 &&
                      !is.na(info$industry[1]) && nzchar(info$industry[1] %||% "")) {
    info$industry[1]
  } else NULL
  
  reach_str <- if (!is.null(info) && nrow(info) > 0) {
    format_reach_display(info$brand_reach[1], info$reach_country[1],
                         info$reach_region[1], info$reach_postcode[1])
  } else "Global"
  
  div(
    style = "margin: 16px 12px 8px; padding: 14px;
             background: rgba(212,168,67,0.08);
             border: 1px solid rgba(212,168,67,0.25);
             border-radius: 10px;",
    
    # Brand name
    div(
      style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
      div(
        style = "width: 28px; height: 28px; border-radius: 7px;
                 background: rgba(212,168,67,0.2); display: flex;
                 align-items: center; justify-content: center; flex-shrink: 0;",
        icon("building", style = "font-size: 12px; color: #D4A843;")
      ),
      div(
        style = "font-size: 13px; font-weight: 700; color: #D4A843;
                 white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
        rv$brand_name
      )
    ),
    
    # Industry
    if (!is.null(industry_str)) {
      div(
        style = "display: flex; align-items: flex-start; gap: 6px; margin-bottom: 4px;",
        icon("industry",
             style = "font-size: 10px; color: #9E9E9E; margin-top: 2px; flex-shrink: 0;"),
        div(
          style = "font-size: 11px; color: #9E9E9E; line-height: 1.4;",
          industry_str
        )
      )
    },
    
    # Reach
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      icon("globe",
           style = "font-size: 10px; color: #9E9E9E; flex-shrink: 0;"),
      div(
        style = "font-size: 11px; color: #9E9E9E;",
        reach_str
      )
    )
  )
})

# ============================================
# Sticky score bar — shown when score cards scroll out of view
# ============================================

output$sticky_score_bar_ui <- renderUI({
  req(rv$logged_in, rv$login_id, rv$brand_name)
  
  scores <- tryCatch(dash_latest_scores(), error = function(e) NULL)
  if (is.null(scores) || nrow(scores) == 0) return(NULL)
  
  main <- scores %>% filter(main_brand_flag == TRUE)
  if (nrow(main) == 0) return(NULL)
  
  fmt <- function(val, digits = 0) {
    if (is.na(val) || is.null(val) || val == 0) return("NA")
    round(val, digits)
  }
  
  val_color <- function(val) {
    if (is.na(val) || is.null(val) || val == 0) return("#718096")
    if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
  }
  
  div(
    style = "background: rgba(212,168,67,0.06);
             border: 1px solid rgba(212,168,67,0.2);
             border-radius: 10px; padding: 10px 12px;
             margin-top: 8px;",
    
    # Header row
    div(
      style = "display: flex; align-items: center; gap: 6px; margin-bottom: 10px;",
      icon("chart-line", style = "font-size: 10px; color: #D4A843;"),
      tags$span(
        style = "font-size: 10px; font-weight: 700; text-transform: uppercase;
                 letter-spacing: 0.8px; color: #D4A843;",
        "Current Scores"
      ),
      div(
        style = "margin-left: auto; font-size: 10px; color: #9E9E9E;",
        format(main$date[1], "%b %d")
      )
    ),
    
    # AiRR — larger, prominent
    div(
      style = "display: flex; align-items: center; justify-content: space-between;
               padding: 6px 0; border-bottom: 1px solid rgba(212,168,67,0.15);
               margin-bottom: 6px;",
      tags$span(
        style = "font-size: 11px; font-weight: 600; color: #9E9E9E;",
        "AiRR Score"
      ),
      tags$span(
        style = paste0("font-size: 20px; font-weight: 800; color: ",
                       val_color(main$airr_score[1]), ";"),
        fmt(main$airr_score[1], digits = 1)
      )
    ),
    
    # Four P scores — compact rows
    div(
      style = "display: flex; flex-direction: column; gap: 4px;",
      
      # Presence
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "display: flex; align-items: center; gap: 5px;",
          div(style = "width: 6px; height: 6px; border-radius: 50%;
                       background: #27AE60; flex-shrink: 0;"),
          tags$span(style = "font-size: 11px; color: #9E9E9E;", "Presence")
        ),
        tags$span(
          style = "font-size: 13px; font-weight: 700; color: #4a5568;",
          fmt(main$presence_score[1])
        )
      ),
      
      # Perception
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "display: flex; align-items: center; gap: 5px;",
          div(style = "width: 6px; height: 6px; border-radius: 50%;
                       background: #D4A843; flex-shrink: 0;"),
          tags$span(style = "font-size: 11px; color: #9E9E9E;", "Perception")
        ),
        tags$span(
          style = "font-size: 13px; font-weight: 700; color: #4a5568;",
          fmt(main$perception_score[1])
        )
      ),
      
      # Prestige
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "display: flex; align-items: center; gap: 5px;",
          div(style = "width: 6px; height: 6px; border-radius: 50%;
                       background: #8E44AD; flex-shrink: 0;"),
          tags$span(style = "font-size: 11px; color: #9E9E9E;", "Prestige")
        ),
        tags$span(
          style = "font-size: 13px; font-weight: 700; color: #4a5568;",
          fmt(main$prestige_score[1])
        )
      ),
      
      # Persistence
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "display: flex; align-items: center; gap: 5px;",
          div(style = "width: 6px; height: 6px; border-radius: 50%;
                       background: #2980B9; flex-shrink: 0;"),
          tags$span(style = "font-size: 11px; color: #9E9E9E;", "Persistence")
        ),
        tags$span(
          style = "font-size: 13px; font-weight: 700; color: #4a5568;",
          fmt(main$persistence_score[1])
        )
      )
    )
  )
})

