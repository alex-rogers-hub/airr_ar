# ============================================
# Compare brands
# ============================================

# Populate brand dropdowns
observe({
  req(rv$logged_in)
  all_brands <- dbGetQuery(
    pool,
    "SELECT DISTINCT brand_name FROM dim_brand ORDER BY brand_name"
  )$brand_name
  
  updateSelectInput(session, "compare_customer_1", choices = c("Select...", all_brands))
  updateSelectInput(session, "compare_customer_2", choices = c("Select...", all_brands))
  updateSelectInput(session, "compare_customer_3", choices = c("Select...", all_brands))
})

# Get comparison data
comparison_data <- reactive({
  req(rv$logged_in)
  selected_brands <- c(
    input$compare_customer_1,
    input$compare_customer_2,
    input$compare_customer_3
  )
  selected_brands <- selected_brands[selected_brands != "Select..." & !is.null(selected_brands)]
  
  req(length(selected_brands) > 0)
  
  placeholders <- paste0("$", 1:length(selected_brands), collapse = ", ")
  
  query <- sprintf("
    WITH latest_presence AS (
      SELECT db.brand_name, fph.overall_score as presence_score,
        ROW_NUMBER() OVER (PARTITION BY db.brand_id ORDER BY fph.date DESC) as rn
      FROM dim_brand db
      LEFT JOIN fact_presence_history fph ON db.brand_id = fph.brand_id
      WHERE db.brand_name IN (%s) AND fph.overall_score IS NOT NULL
    ),
    latest_perception AS (
      SELECT db.brand_name, fph.perception_score,
        ROW_NUMBER() OVER (PARTITION BY db.brand_id ORDER BY fph.date DESC) as rn
      FROM dim_brand db
      LEFT JOIN fact_perception_history fph ON db.brand_id = fph.brand_id
      WHERE db.brand_name IN (%s) AND fph.perception_score IS NOT NULL
    ),
    latest_prestige AS (
      SELECT db.brand_name, fph.prestige_score,
        ROW_NUMBER() OVER (PARTITION BY db.brand_id ORDER BY fph.date DESC) as rn
      FROM dim_brand db
      LEFT JOIN fact_prestige_history fph ON db.brand_id = fph.brand_id
      WHERE db.brand_name IN (%s) AND fph.prestige_score IS NOT NULL
    ),
    latest_persistence AS (
      SELECT db.brand_name, fph.persistence_score,
        ROW_NUMBER() OVER (PARTITION BY db.brand_id ORDER BY fph.date DESC) as rn
      FROM dim_brand db
      LEFT JOIN fact_persistence_history fph ON db.brand_id = fph.brand_id
      WHERE db.brand_name IN (%s) AND fph.persistence_score IS NOT NULL
    )
    SELECT 
      COALESCE(pres.brand_name, perc.brand_name, prest.brand_name, pers.brand_name) as brand_name,
      COALESCE(pres.presence_score, 0) as presence_score,
      COALESCE(perc.perception_score, 0) as perception_score,
      COALESCE(prest.prestige_score, 0) as prestige_score,
      COALESCE(pers.persistence_score, 0) as persistence_score
    FROM latest_presence pres
    FULL OUTER JOIN latest_perception perc ON pres.brand_name = perc.brand_name AND perc.rn = 1
    FULL OUTER JOIN latest_prestige prest ON COALESCE(pres.brand_name, perc.brand_name) = prest.brand_name AND prest.rn = 1
    FULL OUTER JOIN latest_persistence pers ON COALESCE(pres.brand_name, perc.brand_name, prest.brand_name) = pers.brand_name AND pers.rn = 1
    WHERE pres.rn = 1
  ", placeholders, placeholders, placeholders, placeholders)
  
  result <- dbGetQuery(pool, query, params = as.list(selected_brands))
  return(result)
})

# Spider chart comparison
output$spider_chart_compare <- renderPlotly({
  req(comparison_data())
  
  data <- comparison_data()
  
  p <- plot_ly(type = 'scatterpolar', fill = 'toself')
  
  for (i in 1:nrow(data)) {
    p <- p %>% add_trace(
      r = c(data$presence_score[i], data$perception_score[i],
            data$prestige_score[i], data$persistence_score[i],
            data$presence_score[i]),
      theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
      name = data$brand_name[i],
      line = list(width = 3),
      marker = list(size = 8),
      hovertemplate = '<b>%{fullData.name}</b><br>%{theta}: %{r:.2f}<br><extra></extra>'
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
    plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF"
  ) %>% config(displayModeBar = TRUE)
})