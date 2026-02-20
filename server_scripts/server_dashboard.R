# ============================================
# Dashboard - User's brands with main highlighted
# ============================================

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

# --- Reactive: Latest scores for all user's brands ---
dash_latest_scores <- reactive({
  req(user_all_brand_ids())
  
  brands <- user_all_brand_ids()
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
  
  query <- sprintf("
    WITH latest_dates AS (
      SELECT brand_id, MAX(date) as latest_date
      FROM fact_airr_history
      WHERE brand_id IN (%s) AND airr_score IS NOT NULL
      GROUP BY brand_id
    )
    SELECT 
      db.brand_name,
      db.brand_id,
      fa.airr_score,
      fpres.overall_score as presence_score,
      fperc.perception_score,
      fprest.prestige_score,
      fpers.persistence_score,
      fa.date
    FROM dim_brand db
    INNER JOIN latest_dates ld ON db.brand_id = ld.brand_id
    INNER JOIN fact_airr_history fa ON db.brand_id = fa.brand_id AND fa.date = ld.latest_date
    LEFT JOIN fact_presence_history fpres ON db.brand_id = fpres.brand_id AND fpres.date = ld.latest_date
    LEFT JOIN fact_perception_history fperc ON db.brand_id = fperc.brand_id AND fperc.date = ld.latest_date
    LEFT JOIN fact_prestige_history fprest ON db.brand_id = fprest.brand_id AND fprest.date = ld.latest_date
    LEFT JOIN fact_persistence_history fpers ON db.brand_id = fpers.brand_id AND fpers.date = ld.latest_date
    ORDER BY fa.airr_score DESC
  ", placeholders)
  
  result <- dbGetQuery(pool, query, params = as.list(brand_ids))
  
  result <- result %>%
    left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  
  result
})

# --- Reactive: Timeseries for all user's brands ---
dash_timeseries <- reactive({
  req(user_all_brand_ids())
  
  brands <- user_all_brand_ids()
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
  
  query <- sprintf("
    SELECT db.brand_name, db.brand_id, fa.date, fa.airr_score
    FROM dim_brand db
    LEFT JOIN fact_airr_history fa ON db.brand_id = fa.brand_id
    WHERE db.brand_id IN (%s) AND fa.airr_score IS NOT NULL
    ORDER BY db.brand_name, fa.date
  ", placeholders)
  
  result <- dbGetQuery(pool, query, params = as.list(brand_ids))
  result %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
})

# ============================================
# Score Cards - Combined as one UI output
# ============================================

output$dash_score_cards_row <- renderUI({
  
  scores <- tryCatch(dash_latest_scores(), error = function(e) NULL)
  
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
  
  main_scores <- scores %>% filter(main_brand_flag == TRUE)
  comps <- scores %>% filter(main_brand_flag == FALSE)
  
  # AIRR score
  airr_val <- if (nrow(main_scores) > 0) round(main_scores$airr_score[1], 1) else "—"
  airr_date <- if (nrow(main_scores) > 0) format(main_scores$date[1], "%b %d") else ""
  
  # 4 P scores
  presence_val <- if (nrow(main_scores) > 0) round(main_scores$presence_score[1], 1) else "—"
  perception_val <- if (nrow(main_scores) > 0) round(main_scores$perception_score[1], 1) else "—"
  prestige_val <- if (nrow(main_scores) > 0) round(main_scores$prestige_score[1], 1) else "—"
  persistence_val <- if (nrow(main_scores) > 0) round(main_scores$persistence_score[1], 1) else "—"
  
  # Rank
  main_rank <- if (nrow(main_scores) > 0) {
    which(scores$brand_name == main_scores$brand_name[1])
  } else {
    "—"
  }
  
  fluidRow(
    # AIRR Score card
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
    
    # 4 P's grid
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
    
    # Position summary
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
  
  # Colour palette for competitors
  comp_colours <- c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6", 
                    "#E67E22", "#1ABC9C", "#34495E", "#F39C12",
                    "#16A085", "#C0392B")
  
  p <- plot_ly()
  
  # Competitors with distinct colours
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
  
  # Main brand on top
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
  brands <- user_all_brand_ids()
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.overall_score as presence_score
    FROM dim_brand db
    LEFT JOIN fact_presence_history fph ON db.brand_id = fph.brand_id
    WHERE db.brand_id IN (%s) AND fph.overall_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(brand_ids))
  
  data <- data %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  create_dash_chart(data, "presence_score", "Presence Score", rv$brand_name)
})

output$dash_chart_perception <- renderPlotly({
  req(user_all_brand_ids())
  brands <- user_all_brand_ids()
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.perception_score
    FROM dim_brand db
    LEFT JOIN fact_perception_history fph ON db.brand_id = fph.brand_id
    WHERE db.brand_id IN (%s) AND fph.perception_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(brand_ids))
  
  data <- data %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  create_dash_chart(data, "perception_score", "Perception Score", rv$brand_name)
})

output$dash_chart_prestige <- renderPlotly({
  req(user_all_brand_ids())
  brands <- user_all_brand_ids()
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.prestige_score
    FROM dim_brand db
    LEFT JOIN fact_prestige_history fph ON db.brand_id = fph.brand_id
    WHERE db.brand_id IN (%s) AND fph.prestige_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(brand_ids))
  
  data <- data %>% left_join(brands %>% select(brand_id, main_brand_flag), by = "brand_id")
  create_dash_chart(data, "prestige_score", "Prestige Score", rv$brand_name)
})

output$dash_chart_persistence <- renderPlotly({
  req(user_all_brand_ids())
  brands <- user_all_brand_ids()
  brand_ids <- brands$brand_id
  placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, db.brand_id, fph.date, fph.persistence_score
    FROM dim_brand db
    LEFT JOIN fact_persistence_history fph ON db.brand_id = fph.brand_id
    WHERE db.brand_id IN (%s) AND fph.persistence_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(brand_ids))
  
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
  
  # Competitors with distinct colours and hover
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
  
  # Main brand on top
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

output$dash_rankings_table <- renderDT({
  req(dash_latest_scores())
  
  data <- dash_latest_scores() %>%
    mutate(
      brand_name = ifelse(main_brand_flag, paste0("⭐ ", brand_name), brand_name)
    ) %>%
    select(Brand = brand_name, `AIRR Score` = airr_score,
           Presence = presence_score, Perception = perception_score,
           Prestige = prestige_score, Persistence = persistence_score,
           Date = date)
  
  datatable(
    data,
    options = list(
      pageLength = 20,
      dom = 'tip',
      ordering = TRUE,
      scrollX = TRUE,
      language = list(
        info = "Showing _START_ to _END_ of _TOTAL_ brands",
        emptyTable = "No brand data available"
      ),
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
  
  # Select first query by default
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
  filename = function() {
    paste0("airr_brand_data_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(user_all_brand_ids())
    
    brands <- user_all_brand_ids()
    brand_ids <- brands$brand_id
    placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
    
    data <- dbGetQuery(pool, sprintf("
      SELECT db.brand_name, fa.date, fa.airr_score,
             fpres.overall_score as presence_score,
             fperc.perception_score,
             fprest.prestige_score,
             fpers.persistence_score
      FROM dim_brand db
      LEFT JOIN fact_airr_history fa ON db.brand_id = fa.brand_id
      LEFT JOIN fact_presence_history fpres ON db.brand_id = fpres.brand_id AND fpres.date = fa.date
      LEFT JOIN fact_perception_history fperc ON db.brand_id = fperc.brand_id AND fperc.date = fa.date
      LEFT JOIN fact_prestige_history fprest ON db.brand_id = fprest.brand_id AND fprest.date = fa.date
      LEFT JOIN fact_persistence_history fpers ON db.brand_id = fpers.brand_id AND fpers.date = fa.date
      WHERE db.brand_id IN (%s)
      ORDER BY db.brand_name, fa.date
    ", placeholders), params = as.list(brand_ids))
    
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