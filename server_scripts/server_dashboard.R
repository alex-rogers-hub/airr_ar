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

# Auto-refresh dashboard while scores are being calculated
observe({
  req(rv$logged_in, rv$login_id)
  
  brands <- tryCatch(user_all_brand_ids(), error = function(e) NULL)
  scores <- tryCatch(dash_latest_scores(), error = function(e) NULL)
  
  if (!is.null(brands) && nrow(brands) > 0) {
    brands_with_scores <- if (!is.null(scores)) nrow(scores) else 0
    
    # If not all brands have scores yet, keep polling
    if (brands_with_scores < nrow(brands)) {
      invalidateLater(8000, session)
      rv$brands_refresh <- isolate(rv$brands_refresh) + 1
    }
  }
})

# --- Reactive: Latest scores for all user's brands ---
dash_latest_scores <- reactive({
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
  result %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
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
    # Show placeholder "calculating" cards
    return(
      fluidRow(
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
        column(
          width = 4,
          div(
            class = "score-card-grid",
            div(class = "score-card-mini presence",
                div(class = "score-label", "Presence"),
                div(class = "score-value", 
                    tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;"))),
            div(class = "score-card-mini perception",
                div(class = "score-label", "Perception"),
                div(class = "score-value", 
                    tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;"))),
            div(class = "score-card-mini prestige",
                div(class = "score-label", "Prestige"),
                div(class = "score-value", 
                    tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;"))),
            div(class = "score-card-mini persistence",
                div(class = "score-label", "Persistence"),
                div(class = "score-value", 
                    tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 18px;")))
          )
        ),
        column(
          width = 4,
          div(
            style = "background: white; border-radius: 16px; padding: 25px; height: 100%;
                     min-height: 200px; box-shadow: 0 2px 12px rgba(0,0,0,0.06);
                     display: flex; flex-direction: column; justify-content: center;
                     text-align: center;",
            tags$i(class = "fa fa-spinner fa-spin", 
                   style = "font-size: 24px; color: #667eea; margin-bottom: 12px;"),
            div(style = "font-size: 14px; font-weight: 600; color: #4a5568;",
                "Setting up your dashboard"),
            div(style = "font-size: 12px; color: #a0aec0; margin-top: 6px;",
                paste0(nrow(brands), " brand", 
                       if (nrow(brands) != 1) "s" else "",
                       " being scored...")),
            div(style = "font-size: 11px; color: #cbd5e0; margin-top: 8px;",
                "Scores will appear automatically when ready")
          )
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
  
  # --- Normal score cards (existing code) ---
  main_scores <- scores %>% filter(main_brand_flag == TRUE)
  comps <- scores %>% filter(main_brand_flag == FALSE)
  
  airr_val <- if (nrow(main_scores) > 0) round(main_scores$airr_score[1], 1) else "—"
  airr_date <- if (nrow(main_scores) > 0) format(main_scores$date[1], "%b %d") else ""
  
  presence_val <- if (nrow(main_scores) > 0) round(main_scores$presence_score[1], 1) else "—"
  perception_val <- if (nrow(main_scores) > 0) round(main_scores$perception_score[1], 1) else "—"
  prestige_val <- if (nrow(main_scores) > 0) round(main_scores$prestige_score[1], 1) else "—"
  persistence_val <- if (nrow(main_scores) > 0) round(main_scores$persistence_score[1], 1) else "—"
  
  main_rank <- if (nrow(main_scores) > 0) {
    which(scores$brand_name == main_scores$brand_name[1])
  } else {
    "—"
  }
  
  fluidRow(
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
    column(
      width = 4,
      div(
        class = "score-card-grid",
        div(class = "score-card-mini presence",
            div(class = "score-label", "Presence"),
            div(class = "score-value", presence_val)),
        div(class = "score-card-mini perception",
            div(class = "score-label", "Perception"),
            div(class = "score-value", perception_val)),
        div(class = "score-card-mini prestige",
            div(class = "score-label", "Prestige"),
            div(class = "score-value", prestige_val)),
        div(class = "score-card-mini persistence",
            div(class = "score-label", "Persistence"),
            div(class = "score-value", persistence_val))
      )
    ),
    column(
      width = 4,
      div(
        style = "background: white; border-radius: 16px; padding: 25px; height: 100%;
                 min-height: 200px; box-shadow: 0 2px 12px rgba(0,0,0,0.06);
                 display: flex; flex-direction: column; justify-content: center;",
        div(
          style = "text-align: center;",
          div(
            style = "font-size: 12px; font-weight: 600; text-transform: uppercase; 
                     letter-spacing: 0.5px; color: #718096; margin-bottom: 8px;",
            "Your Position"
          ),
          div(
            style = "font-size: 42px; font-weight: 700; color: #2d3748;",
            paste0("#", main_rank)
          ),
          div(
            style = "font-size: 13px; color: #a0aec0; margin-top: 4px;",
            paste0("of ", nrow(scores), " brands tracked")
          )
        ),
        hr(style = "margin: 15px 0; border-color: #f0f0f0;"),
        div(
          style = "text-align: center;",
          div(
            style = "font-size: 12px; font-weight: 600; text-transform: uppercase; 
                     letter-spacing: 0.5px; color: #718096; margin-bottom: 4px;",
            "Competitors"
          ),
          div(
            style = "font-size: 28px; font-weight: 700; color: #667eea;",
            nrow(comps)
          )
        )
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
      title = "",
      showgrid = FALSE,
      linecolor = "#e2e8f0",
      tickfont = list(size = 11, color = "#9E9E9E", family = "Inter"),
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(text = y_title, 
                   font = list(size = 12, color = "#9E9E9E", family = "Inter")),
      showgrid = TRUE,
      gridcolor = "rgba(0, 0, 0, 0.05)",
      gridwidth = 1,
      linecolor = "#e2e8f0",
      tickfont = list(size = 11, color = "#9E9E9E", family = "Inter"),
      zeroline = FALSE
    ),
    hovermode = 'x unified',
    plot_bgcolor = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 50, r = 20, t = 10, b = 40),
    legend = list(
      orientation = "h",
      yanchor = "top",
      y = -0.15,
      xanchor = "center",
      x = 0.5,
      font = list(size = 11, color = "#9E9E9E", family = "Inter"),
      bgcolor = "rgba(0,0,0,0)"
    ),
    showlegend = show_legend,
    font = list(family = "Inter")
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
  
  p <- plot_ly()
  
  if (nrow(comp_data) > 0) {
    comp_brands <- unique(comp_data$brand_name)
    for (idx in seq_along(comp_brands)) {
      bn <- comp_brands[idx]
      bd <- comp_data %>% filter(brand_name == bn)
      col <- comp_colours[((idx - 1) %% length(comp_colours)) + 1]
      
      p <- p %>% add_trace(
        data = bd, x = ~date, y = as.formula(paste0("~", score_col)),
        type = 'scatter', mode = 'lines+markers',
        name = bn,
        line = list(width = 2, dash = "dot", shape = "spline", color = col),
        marker = list(size = 4, color = col),
        opacity = 0.7,
        hovertemplate = paste0('<b>', bn, '</b><br>%{y:.1f}<extra></extra>')
      )
    }
  }
  
  if (nrow(main_data) > 0) {
    p <- p %>% add_trace(
      data = main_data, x = ~date, y = as.formula(paste0("~", score_col)),
      type = 'scatter', mode = 'lines+markers',
      name = paste0("★ ", main_brand_name),
      line = list(width = 3.5, color = '#D4A843', shape = "spline"),
      marker = list(size = 7, color = '#D4A843',
                    line = list(color = '#1A1A1A', width = 1.5)),
      hovertemplate = paste0('<b>', main_brand_name, '</b><br>%{y:.1f}<extra></extra>')
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
  login_id  <- rv$login_id                           # NEW
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.overall_score as presence_score
    FROM dim_brand db
    LEFT JOIN fact_presence_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1                          -- NEW
    WHERE db.brand_id IN (%s) AND fph.overall_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  create_dash_chart(data, "presence_score", "Presence Score", rv$brand_name)
})

output$dash_chart_perception <- renderPlotly({
  req(user_all_brand_ids())
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id                           # NEW
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.perception_score
    FROM dim_brand db
    LEFT JOIN fact_perception_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1                          -- NEW
    WHERE db.brand_id IN (%s) AND fph.perception_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  create_dash_chart(data, "perception_score", "Perception Score", rv$brand_name)
})

output$dash_chart_prestige <- renderPlotly({
  req(user_all_brand_ids())
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id                           # NEW
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.prestige_score
    FROM dim_brand db
    LEFT JOIN fact_prestige_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1                          -- NEW
    WHERE db.brand_id IN (%s) AND fph.prestige_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  create_dash_chart(data, "prestige_score", "Prestige Score", rv$brand_name)
})

output$dash_chart_persistence <- renderPlotly({
  req(user_all_brand_ids())
  brands    <- user_all_brand_ids()
  login_id  <- rv$login_id                           # NEW
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.persistence_score
    FROM dim_brand db
    LEFT JOIN fact_persistence_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1                          -- NEW
    WHERE db.brand_id IN (%s) AND fph.persistence_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(login_id, brand_ids)))
  
  data <- data %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
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
      val <- round(val, 1)
      div(
        style = paste0(
          "flex: 0 0 80px; text-align: center; font-size: 13px; font-weight: 600; color: ", 
          score_color(val), ";"
        ),
        val
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
    login_id  <- rv$login_id                         # NEW
    brand_ids <- brands$brand_id
    placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
    
    data <- dbGetQuery(pool, sprintf("
      SELECT db.brand_name, fa.date, fa.airr_score,
             fpres.overall_score  as presence_score,
             fperc.perception_score,
             fprest.prestige_score,
             fpers.persistence_score
      FROM dim_brand db
      LEFT JOIN fact_airr_history fa
        ON db.brand_id = fa.brand_id AND fa.login_id = $1         -- NEW
      LEFT JOIN fact_presence_history fpres
        ON db.brand_id = fpres.brand_id AND fpres.date = fa.date
        AND fpres.login_id = $1                                    -- NEW
      LEFT JOIN fact_perception_history fperc
        ON db.brand_id = fperc.brand_id AND fperc.date = fa.date
        AND fperc.login_id = $1                                    -- NEW
      LEFT JOIN fact_prestige_history fprest
        ON db.brand_id = fprest.brand_id AND fprest.date = fa.date
        AND fprest.login_id = $1                                   -- NEW
      LEFT JOIN fact_persistence_history fpers
        ON db.brand_id = fpers.brand_id AND fpers.date = fa.date
        AND fpers.login_id = $1                                    -- NEW
      WHERE db.brand_id IN (%s)
      ORDER BY db.brand_name, fa.date
    ", placeholders), params = as.list(c(login_id, brand_ids)))
    
    write.csv(data, file, row.names = FALSE)
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
  filename = function() {
    paste0("airr_prompt_data_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(dash_query_timeseries())
    write.csv(dash_query_timeseries() %>%
                select(-brand_id, -main_brand_flag), file, row.names = FALSE)
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
      val <- round(val, 1)
      div(
        style = paste0("flex: 0 0 80px; text-align: center; font-size: 13px; font-weight: 600; color: ",
                       score_color(val), ";"),
        val
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

render_compact_rankings <- function(data) {
  
  if (nrow(data) == 0) {
    return(div(
      style = "text-align: center; padding: 30px; color: #a0aec0;",
      icon("chart-bar", class = "fa-2x", style = "margin-bottom: 10px;"),
      p("No data yet")
    ))
  }
  
  score_color <- function(val) {
    if (is.na(val) || is.null(val)) return("#cbd5e0")
    if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
  }
  
  score_cell <- function(val, col) {
    div(
      style = paste0(
        "flex: 0 0 38px; text-align: center; font-size: 12px; ",
        "font-weight: 700; color: ", col, ";"
      ),
      round(val, 0)
    )
  }
  
  rows <- lapply(1:nrow(data), function(i) {
    row      <- data[i, ]
    is_main  <- isTRUE(row$main_brand_flag)
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
      "background: linear-gradient(90deg, rgba(102,126,234,0.08) 0%, 
       rgba(102,126,234,0.03) 100%); border-left: 3px solid #667eea;"
    } else {
      "border-left: 3px solid transparent;"
    }
    
    div(
      style = paste0(
        "display: flex; align-items: center; padding: 7px 10px; ",
        "border-bottom: 1px solid #f0f0f0; gap: 6px; ", row_bg
      ),
      
      # Rank badge
      div(
        style = paste0(
          "flex: 0 0 22px; height: 22px; border-radius: 50%; display: flex; ",
          "align-items: center; justify-content: center; font-weight: 700; ",
          "font-size: 10px; flex-shrink: 0; ", rank_style
        ),
        i
      ),
      
      # Brand name
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
          style = "background: #667eea; color: white; font-size: 7px; 
                   padding: 1px 4px; border-radius: 5px; font-weight: 600;
                   white-space: nowrap; flex-shrink: 0; line-height: 1.6;",
          "YOU"
        )
      ),
      
      # AIRR score
      div(
        style = paste0(
          "flex: 0 0 40px; text-align: right; font-size: 16px; font-weight: 800; ",
          "color: ", score_color(airr_val), "; flex-shrink: 0;"
        ),
        airr_val
      ),
      
      # Divider
      div(style = "flex: 0 0 1px; height: 24px; background: #e2e8f0; flex-shrink: 0;"),
      
      # 4 P scores
      score_cell(row$presence_score,    BRAND_THEME$presence),
      score_cell(row$perception_score,  BRAND_THEME$perception),
      score_cell(row$prestige_score,    BRAND_THEME$prestige),
      score_cell(row$persistence_score, BRAND_THEME$persistence)
    )
  })
  
  div(
    # Header row
    div(
      style = "display: flex; align-items: center; padding: 7px 10px; gap: 6px;
               background: linear-gradient(135deg, #1a202c 0%, #2d3748 100%);
               border-radius: 8px 8px 0 0;",
      
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
      
      div(style = "flex: 0 0 1px; height: 16px; background: #4a5568; flex-shrink: 0;"),
      
      div(
        style = "flex: 0 0 38px; text-align: center;",
        tags$span(
          style = "color: #27AE60; font-size: 8px; font-weight: 600;
                   text-transform: uppercase; letter-spacing: 0.5px;",
          "Pres"
        )
      ),
      div(
        style = "flex: 0 0 38px; text-align: center;",
        tags$span(
          style = "color: #D4A843; font-size: 8px; font-weight: 600;
                   text-transform: uppercase; letter-spacing: 0.5px;",
          "Perc"
        )
      ),
      div(
        style = "flex: 0 0 38px; text-align: center;",
        tags$span(
          style = "color: #8E44AD; font-size: 8px; font-weight: 600;
                   text-transform: uppercase; letter-spacing: 0.5px;",
          "Prest"
        )
      ),
      div(
        style = "flex: 0 0 38px; text-align: center;",
        tags$span(
          style = "color: #2980B9; font-size: 8px; font-weight: 600;
                   text-transform: uppercase; letter-spacing: 0.5px;",
          "Pers"
        )
      )
    ),
    
    # Rows
    div(
      style = "border: 1px solid #e2e8f0; border-top: none;
               border-radius: 0 0 8px 8px; overflow: hidden;",
      do.call(tagList, rows)
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
              "Customer Profiles — Enterprise Feature"),
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
        p("No profiles set up yet."),
        actionButton("manage_profiles_from_brand2", "Add Profiles",
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