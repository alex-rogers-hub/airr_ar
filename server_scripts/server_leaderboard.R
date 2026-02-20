# ============================================
# Leaderboard
# ============================================

leaderboard_data <- reactive({
  req(rv$logged_in)
  query <- "
    WITH latest_dates AS (
      SELECT brand_id, MAX(date) as latest_date
      FROM fact_airr_history
      WHERE airr_score IS NOT NULL
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
    INNER JOIN fact_airr_history fa ON db.brand_id = fa.brand_id AND fa.date = ld.latest_date
    LEFT JOIN fact_presence_history fpres ON db.brand_id = fpres.brand_id AND fpres.date = ld.latest_date
    LEFT JOIN fact_perception_history fperc ON db.brand_id = fperc.brand_id AND fperc.date = ld.latest_date
    LEFT JOIN fact_prestige_history fprest ON db.brand_id = fprest.brand_id AND fprest.date = ld.latest_date
    LEFT JOIN fact_persistence_history fpers ON db.brand_id = fpers.brand_id AND fpers.date = ld.latest_date
    ORDER BY fa.airr_score DESC
  "
  dbGetQuery(pool, query)
})

output$leaderboard_table <- renderDT({
  req(leaderboard_data())
  
  datatable(
    leaderboard_data(),
    options = list(pageLength = 25, order = list(list(1, 'desc')), dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'excel'), scrollX = TRUE),
    rownames = FALSE, class = 'cell-border stripe'
  ) %>%
    formatRound(c('airr_score', 'presence_score', 'perception_score', 'prestige_score', 'persistence_score'), 2) %>%
    formatDate('date', 'toDateString')
})

leaderboard_timeseries <- reactive({
  req(rv$logged_in)
  query <- "
    SELECT db.brand_name, fa.date, fa.airr_score
    FROM dim_brand db
    LEFT JOIN fact_airr_history fa ON db.brand_id = fa.brand_id
    WHERE fa.airr_score IS NOT NULL
    ORDER BY db.brand_name, fa.date
  "
  dbGetQuery(pool, query)
})

# Helper to create leaderboard chart
create_leaderboard_chart <- function(data, score_col, y_title) {
  top_brands <- leaderboard_data() %>% head(10) %>% pull(brand_name)
  data_filtered <- data %>% filter(brand_name %in% top_brands)
  
  plot_ly() %>%
    add_trace(
      data = data_filtered, x = ~date, y = as.formula(paste0("~", score_col)),
      color = ~brand_name, type = 'scatter', mode = 'lines+markers',
      line = list(width = 2), marker = list(size = 2),
      hovertemplate = paste0('<b>%{fullData.name}</b><br>Date: %{x|%Y-%m-%d}<br>', y_title, ': %{y:.2f}<br><extra></extra>')
    ) %>%
    layout(
      xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
      yaxis = list(title = y_title, showgrid = TRUE, gridcolor = "#E8EDF2"),
      hovermode = 'closest', plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
      margin = list(l = 50, r = 150, t = 30, b = 80),
      legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
      showlegend = TRUE
    ) %>% config(displayModeBar = TRUE)
}

output$leaderboard_chart_airr <- renderPlotly({
  req(leaderboard_timeseries())
  create_leaderboard_chart(leaderboard_timeseries(), "airr_score", "AIRR Score")
})

output$leaderboard_chart_presence <- renderPlotly({
  req(leaderboard_timeseries())
  data <- dbGetQuery(pool, "
    SELECT db.brand_name, fph.date, fph.overall_score as presence_score
    FROM dim_brand db
    LEFT JOIN fact_presence_history fph ON db.brand_id = fph.brand_id
    WHERE fph.overall_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ")
  create_leaderboard_chart(data, "presence_score", "Presence Score")
})

output$leaderboard_chart_perception <- renderPlotly({
  req(leaderboard_timeseries())
  data <- dbGetQuery(pool, "
    SELECT db.brand_name, fph.date, fph.perception_score
    FROM dim_brand db
    LEFT JOIN fact_perception_history fph ON db.brand_id = fph.brand_id
    WHERE fph.perception_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ")
  create_leaderboard_chart(data, "perception_score", "Perception Score")
})

output$leaderboard_chart_prestige <- renderPlotly({
  req(leaderboard_timeseries())
  data <- dbGetQuery(pool, "
    SELECT db.brand_name, fph.date, fph.prestige_score
    FROM dim_brand db
    LEFT JOIN fact_prestige_history fph ON db.brand_id = fph.brand_id
    WHERE fph.prestige_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ")
  create_leaderboard_chart(data, "prestige_score", "Prestige Score")
})

output$leaderboard_chart_persistence <- renderPlotly({
  req(leaderboard_timeseries())
  data <- dbGetQuery(pool, "
    SELECT db.brand_name, fph.date, fph.persistence_score
    FROM dim_brand db
    LEFT JOIN fact_persistence_history fph ON db.brand_id = fph.brand_id
    WHERE fph.persistence_score IS NOT NULL
    ORDER BY db.brand_name, fph.date
  ")
  create_leaderboard_chart(data, "persistence_score", "Persistence Score")
})