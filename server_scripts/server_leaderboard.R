# ============================================
# Leaderboard
# ============================================

leaderboard_data <- reactive({
  req(rv$logged_in, rv$login_id)
  query <- "
    WITH latest_dates AS (
      SELECT brand_id, MAX(date) as latest_date
      FROM fact_airr_history
      WHERE login_id = $1 AND airr_score IS NOT NULL
      GROUP BY brand_id
    )
    SELECT 
      db.brand_name,
      fa.airr_score,
      fpres.overall_score as presence_score,
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
  "
  dbGetQuery(pool, query, params = list(rv$login_id))
})

output$leaderboard_table <- renderDT({
  req(leaderboard_data())
  
  datatable(
    leaderboard_data(),
    options = list(pageLength = 25, order = list(list(1, 'desc')), dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'excel'), scrollX = TRUE),
    rownames = FALSE, class = 'cell-border stripe'
  ) %>%
    formatRound(c('airr_score', 'presence_score', 'perception_score', 
                  'prestige_score', 'persistence_score'), 2) %>%
    formatDate('date', 'toDateString')
})

leaderboard_timeseries <- reactive({
  req(rv$logged_in, rv$login_id)
  
  # Get the user's tracked brand IDs first
  brand_ids <- dbGetQuery(pool, "
    SELECT DISTINCT b.brand_id
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  ", params = list(rv$login_id))$brand_id
  
  if (length(brand_ids) == 0) return(data.frame())
  
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  query <- sprintf("
    SELECT db.brand_name, fa.date, fa.airr_score
    FROM dim_brand db
    LEFT JOIN fact_airr_history fa 
      ON db.brand_id = fa.brand_id
      AND fa.login_id = $1
    WHERE db.brand_id IN (%s)
      AND fa.airr_score IS NOT NULL
    ORDER BY db.brand_name, fa.date
  ", placeholders)
  
  dbGetQuery(pool, query, params = as.list(c(rv$login_id, brand_ids)))
})

# Helper to create leaderboard chart — filters to user's top 10 brands
create_leaderboard_chart <- function(data, score_col, y_title) {
  req(leaderboard_data())
  
  top_brands    <- leaderboard_data() %>% head(10) %>% pull(brand_name)
  data_filtered <- data %>% filter(brand_name %in% top_brands)
  
  if (nrow(data_filtered) == 0) {
    return(
      plot_ly() %>%
        layout(
          annotations = list(
            text = "No data available", xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16, color = "#999")
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }
  
  plot_ly() %>%
    add_trace(
      data = data_filtered,
      x = ~date,
      y = as.formula(paste0("~", score_col)),
      color = ~brand_name,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      marker = list(size = 2),
      hovertemplate = paste0(
        '<b>%{fullData.name}</b><br>',
        'Date: %{x|%Y-%m-%d}<br>',
        y_title, ': %{y:.2f}<br><extra></extra>'
      )
    ) %>%
    layout(
      xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
      yaxis = list(title = y_title, showgrid = TRUE, gridcolor = "#E8EDF2"),
      hovermode = 'closest',
      plot_bgcolor = "#FFFFFF",
      paper_bgcolor = "#FFFFFF",
      margin = list(l = 50, r = 150, t = 30, b = 80),
      legend = list(
        orientation = "v", yanchor = "top", y = 1,
        xanchor = "left", x = 1.02
      ),
      showlegend = TRUE
    ) %>%
    config(displayModeBar = TRUE)
}

output$leaderboard_chart_airr <- renderPlotly({
  req(leaderboard_timeseries())
  create_leaderboard_chart(leaderboard_timeseries(), "airr_score", "AIRR Score")
})

output$leaderboard_chart_presence <- renderPlotly({
  req(rv$logged_in, rv$login_id)
  
  brand_ids <- dbGetQuery(pool, "
    SELECT DISTINCT b.brand_id
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  ", params = list(rv$login_id))$brand_id
  
  if (length(brand_ids) == 0) return(NULL)
  
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, fph.date, fph.overall_score as presence_score
    FROM dim_brand db
    LEFT JOIN fact_presence_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1
    WHERE db.brand_id IN (%s)
      AND fph.overall_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(rv$login_id, brand_ids)))
  
  create_leaderboard_chart(data, "presence_score", "Presence Score")
})

output$leaderboard_chart_perception <- renderPlotly({
  req(rv$logged_in, rv$login_id)
  
  brand_ids <- dbGetQuery(pool, "
    SELECT DISTINCT b.brand_id
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  ", params = list(rv$login_id))$brand_id
  
  if (length(brand_ids) == 0) return(NULL)
  
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, fph.date, fph.perception_score
    FROM dim_brand db
    LEFT JOIN fact_perception_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1
    WHERE db.brand_id IN (%s)
      AND fph.perception_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(rv$login_id, brand_ids)))
  
  create_leaderboard_chart(data, "perception_score", "Perception Score")
})

output$leaderboard_chart_prestige <- renderPlotly({
  req(rv$logged_in, rv$login_id)
  
  brand_ids <- dbGetQuery(pool, "
    SELECT DISTINCT b.brand_id
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  ", params = list(rv$login_id))$brand_id
  
  if (length(brand_ids) == 0) return(NULL)
  
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, fph.date, fph.prestige_score
    FROM dim_brand db
    LEFT JOIN fact_prestige_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1
    WHERE db.brand_id IN (%s)
      AND fph.prestige_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(rv$login_id, brand_ids)))
  
  create_leaderboard_chart(data, "prestige_score", "Prestige Score")
})

output$leaderboard_chart_persistence <- renderPlotly({
  req(rv$logged_in, rv$login_id)
  
  brand_ids <- dbGetQuery(pool, "
    SELECT DISTINCT b.brand_id
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
  ", params = list(rv$login_id))$brand_id
  
  if (length(brand_ids) == 0) return(NULL)
  
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  data <- dbGetQuery(pool, sprintf("
    SELECT db.brand_name, fph.date, fph.persistence_score
    FROM dim_brand db
    LEFT JOIN fact_persistence_history fph
      ON db.brand_id = fph.brand_id
      AND fph.login_id = $1
    WHERE db.brand_id IN (%s)
      AND fph.persistence_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ", placeholders), params = as.list(c(rv$login_id, brand_ids)))
  
  create_leaderboard_chart(data, "persistence_score", "Persistence Score")
})