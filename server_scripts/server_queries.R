# ============================================
# Queries - add prompts, query panels, query charts
# ============================================

# Submit prompt for current brand
observeEvent(input$submit_prompt_btn, {
  req(input$prompt_input)
  req(rv$brand_id)
  
  addPrompt(input$prompt_input, rv$brand_id)
  
  rv$queries_refresh <- rv$queries_refresh + 1
  
  updateTextInput(session, "prompt_input", value = "")
  
  showNotification("Prompt submitted successfully!", type = "message", duration = 3)
})

# Get brand queries reactive
brand_queries <- reactive({
  req(rv$logged_in, rv$brand_id)
  rv$queries_refresh
  get_brand_queries(rv$brand_id)
})

# Queries summary
output$queries_summary <- renderUI({
  req(brand_queries())
  
  queries <- brand_queries()
  
  if (nrow(queries) == 0) {
    return(
      div(
        style = "padding: 40px; text-align: center;",
        icon("search", class = "fa-3x", style = "color: #ccc; margin-bottom: 20px;"),
        h4("No queries tracked yet"),
        p("Add a query to start tracking AiRR scores")
      )
    )
  }
  
  tagList(
    div(
      style = "padding: 15px;",
      h4(paste("Tracking", nrow(queries), "queries")),
      p(paste("Last added:", format(max(queries$date_added), "%B %d, %Y")))
    )
  )
})

# Dynamic query panels
output$query_panels <- renderUI({
  req(brand_queries())
  req(rv$brand_id)
  
  queries <- brand_queries()
  brand_id <- rv$brand_id
  
  if (nrow(queries) == 0) return(NULL)
  
  query_panels <- lapply(1:nrow(queries), function(i) {
    query_id <- queries$query_id[i]
    query_string <- queries$query_string[i]
    date_added <- queries$date_added[i]
    
    valuebox_id <- paste0("query_valuebox_", query_id)
    spider_id <- paste0("query_spider_", query_id)
    timeseries_id <- paste0("query_timeseries_", query_id)
    
    fluidRow(
      box(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(strong(paste0("Query ", i, ": ")), query_string),
          span(
            style = "font-size: 12px; color: #7f8c8d; font-weight: normal;",
            paste("Added:", format(date_added, "%b %d, %Y"))
          )
        ),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = (i > 1),
        
        fluidRow(
          column(width = 3, valueBoxOutput(valuebox_id, width = 12)),
          column(
            width = 5,
            box(
              title = "AiRR Score Over Time", status = "info", solidHeader = TRUE, width = 12,
              shinycssloaders::withSpinner(plotlyOutput(timeseries_id, height = "350px"), type = 4, color = "#667eea")
            )
          ),
          column(
            width = 4,
            box(
              title = "Four P's Breakdown", status = "info", solidHeader = TRUE, width = 12,
              shinycssloaders::withSpinner(plotlyOutput(spider_id, height = "350px"), type = 4, color = "#667eea")
            )
          )
        )
      )
    )
  })
  
  do.call(tagList, query_panels)
})

# Create dynamic outputs for each query
observe({
  req(brand_queries())
  req(rv$brand_id)
  
  queries <- brand_queries()
  brand_id <- rv$brand_id
  
  if (nrow(queries) == 0) return()
  
  lapply(1:nrow(queries), function(i) {
    query_id <- queries$query_id[i]
    
    # Value Box
    output[[paste0("query_valuebox_", query_id)]] <- renderValueBox({
      latest_scores <- get_latest_query_scores(query_id, brand_id)
      
      valueBox(
        value = ifelse(
          is.null(latest_scores$airr_score) || latest_scores$airr_score == 0,
          "N/A",
          format(round(latest_scores$airr_score, 1), nsmall = 1)
        ),
        subtitle = "Current AiRR Score",
        icon = icon("chart-line"),
        color = "purple",
        width = 12
      )
    })
    
    # Spider Chart
    output[[paste0("query_spider_", query_id)]] <- renderPlotly({
      latest_scores <- get_latest_query_scores(query_id, brand_id)
      
      if (is.null(latest_scores$airr_score) || latest_scores$airr_score == 0) {
        plot_ly() %>%
          layout(
            annotations = list(text = "No data available", xref = "paper", yref = "paper",
                               x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16, color = "#999")),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
          )
      } else {
        spider_data <- data.frame(
          category = c("Presence", "Perception", "Prestige", "Persistence"),
          value = c(latest_scores$presence_score, latest_scores$perception_score,
                    latest_scores$prestige_score, latest_scores$persistence_score)
        )
        spider_data$normalized <- pmin(100, pmax(0, spider_data$value))
        
        plot_ly(
          type = 'scatterpolar', r = spider_data$normalized, theta = spider_data$category,
          fill = 'toself', fillcolor = 'rgba(102, 126, 234, 0.5)',
          line = list(color = '#667eea', width = 3),
          marker = list(size = 10, color = '#667eea'),
          hovertemplate = '<b>%{theta}</b><br>Score: %{r:.1f}/100<br><extra></extra>'
        ) %>%
          layout(
            polar = list(
              radialaxis = list(visible = TRUE, range = c(0, 100), showgrid = TRUE,
                                gridcolor = "#E8EDF2", tickfont = list(size = 10)),
              angularaxis = list(showgrid = TRUE, gridcolor = "#E8EDF2",
                                 tickfont = list(size = 11, family = "Poppins")),
              bgcolor = "#FFFFFF"
            ),
            paper_bgcolor = "#FFFFFF",
            margin = list(l = 60, r = 60, t = 30, b = 30),
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
      }
    })
    
    # Time Series Chart
    output[[paste0("query_timeseries_", query_id)]] <- renderPlotly({
      history <- get_query_history(query_id)
      
      if (nrow(history) == 0) {
        plot_ly() %>%
          layout(
            annotations = list(text = "No historical data available", xref = "paper", yref = "paper",
                               x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16, color = "#999")),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
          )
      } else {
        min_score <- min(history$airr_score, na.rm = TRUE)
        y_min <- max(0, min_score - 20)
        y_max <- 100
        
        plot_ly() %>%
          add_trace(
            data = history, x = ~date, y = ~airr_score,
            type = 'scatter', mode = 'lines+markers', name = 'AiRR Score',
            line = list(width = 3, color = '#667eea'),
            marker = list(size = 8, color = '#667eea'),
            hovertemplate = '<b>AiRR Score</b><br>Date: %{x|%Y-%m-%d}<br>Score: %{y:.2f}<br><extra></extra>'
          ) %>%
          layout(
            title = list(text = "", font = list(size = 16, color = app_colors$dark)),
            xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "#E8EDF2"),
            yaxis = list(title = "AiRR Score", showgrid = TRUE, gridcolor = "#E8EDF2", range = c(y_min, y_max)),
            hovermode = 'closest',
            plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
            margin = list(l = 50, r = 50, t = 30, b = 50)
          ) %>%
          config(displayModeBar = FALSE)
      }
    })
  })
})

# Submit prompt for all brands
observeEvent(input$submit_prompt_to_all_btn, {
  req(input$new_prompt_input)
  
  query_text <- trimws(input$new_prompt_input)
  
  if (query_text == "") {
    showNotification(ui = "Please enter a query before submitting.", type = "error", duration = 3)
    return()
  }
  
  notification_id <- showNotification(
    ui = "Adding query for all brands...", type = "message", duration = NULL, closeButton = FALSE
  )
  
  isolate({
    result <- tryCatch({
      query_id <- addPromptForEveryone(query_text)
      list(success = TRUE, query_id = query_id)
    }, error = function(e) {
      list(success = FALSE, error = as.character(e$message))
    })
  })
  
  removeNotification(id = notification_id)
  Sys.sleep(0.1)
  
  if (isTRUE(result$success)) {
    showNotification(ui = "Query successfully added for all brands!", type = "message", duration = 5, closeButton = TRUE)
    
    updateTextInput(session, "new_prompt_input", value = "")
    
    all_queries <- dbGetQuery(pool, "SELECT query_id, query_string FROM dim_query ORDER BY query_string")
    query_choices <- setNames(all_queries$query_string, all_queries$query_string)
    
    updateSelectInput(session, "query_select",
                      choices = c("Select a query..." = "", query_choices),
                      selected = query_text)
  } else {
    error_text <- if (is.null(result$error)) "Unknown error" else result$error
    showNotification(ui = paste("Error adding query:", error_text), type = "error", duration = 10, closeButton = TRUE)
  }
})

# Populate query dropdown
observe({
  all_queries <- dbGetQuery(pool, "SELECT query_id, query_string FROM dim_query ORDER BY query_string")
  query_choices <- setNames(all_queries$query_string, all_queries$query_string)
  updateSelectInput(session, "query_select", choices = c("Select a query..." = "", query_choices))
})

# Query timeseries data
query_timeseries <- reactive({
  req(input$query_select)
  req(input$query_select != "")
  
  query <- "
    SELECT 
      db.brand_name,
      fqh.date,
      fqh.airr_score,
      fqh.presence_score,
      fqh.perception_score,
      fqh.prestige_score,
      fqh.persistence_score
    FROM fact_query_history fqh
    INNER JOIN dim_brand db ON fqh.brand_id = db.brand_id
    INNER JOIN dim_query dq ON fqh.query_id = dq.query_id
    WHERE dq.query_string = $1
      AND fqh.airr_score IS NOT NULL
    ORDER BY db.brand_name, fqh.date
  "
  
  dbGetQuery(pool, query, params = list(input$query_select))
})

# Query top 10 data
query_top10_data <- reactive({
  req(input$query_select)
  req(input$query_select != "")
  
  query <- "
    WITH latest_scores AS (
      SELECT 
        db.brand_name,
        fqh.brand_id,
        fqh.airr_score,
        fqh.presence_score,
        fqh.perception_score,
        fqh.prestige_score,
        fqh.persistence_score,
        fqh.date,
        ROW_NUMBER() OVER (PARTITION BY fqh.brand_id ORDER BY fqh.date DESC) as rn
      FROM fact_query_history fqh
      INNER JOIN dim_brand_query dbq ON fqh.brand_id = dbq.brand_id 
        AND fqh.query_id = dbq.query_id
      INNER JOIN dim_brand db ON fqh.brand_id = db.brand_id
      INNER JOIN dim_query dq ON fqh.query_id = dq.query_id
      WHERE dq.query_string = $1
        AND fqh.airr_score IS NOT NULL
    )
    SELECT 
      brand_name,
      airr_score,
      presence_score,
      perception_score,
      prestige_score,
      persistence_score,
      date
    FROM latest_scores
    WHERE rn = 1
    ORDER BY airr_score DESC
    LIMIT 10
  "
  
  dbGetQuery(pool, query, params = list(input$query_select))
})

output$query_top10_table <- renderDT({
  req(query_top10_data())
  
  datatable(
    query_top10_data(),
    options = list(pageLength = 10, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'),
                   scrollX = TRUE, ordering = FALSE),
    rownames = FALSE, class = 'cell-border stripe'
  ) %>%
    formatRound(c('airr_score', 'presence_score', 'perception_score', 'prestige_score', 'persistence_score'), 2) %>%
    formatDate('date', 'toDateString')
})

# Query score charts
output$query_chart_airr <- renderPlotly({
  req(query_timeseries(), query_top10_data())
  data <- query_timeseries()
  top_brands <- query_top10_data() %>% head(10) %>% pull(brand_name)
  data_filtered <- data %>% filter(brand_name %in% top_brands)
  
  plot_ly() %>%
    add_trace(data = data_filtered, x = ~date, y = ~airr_score, color = ~brand_name,
              type = 'scatter', mode = 'lines+markers', line = list(width = 2), marker = list(width = 2),
              hovertemplate = '<b>%{fullData.name}</b><br>Date: %{x|%Y-%m-%d}<br>AIRR Score: %{y:.2f}<br><extra></extra>'
    ) %>%
    layout(xaxis = list(title = "Date", showgrid = FALSE), yaxis = list(title = "AIRR Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
           hovermode = 'closest', plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
           margin = list(l = 50, r = 150, t = 30, b = 80),
           legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02), showlegend = TRUE
    ) %>% config(displayModeBar = TRUE)
})

output$query_chart_presence <- renderPlotly({
  req(query_timeseries(), query_top10_data())
  data <- query_timeseries()
  top_brands <- query_top10_data() %>% head(10) %>% pull(brand_name)
  data_filtered <- data %>% filter(brand_name %in% top_brands)
  
  plot_ly() %>%
    add_trace(data = data_filtered, x = ~date, y = ~presence_score, color = ~brand_name,
              type = 'scatter', mode = 'lines+markers', line = list(width = 2), marker = list(width = 2),
              hovertemplate = '<b>%{fullData.name}</b><br>Date: %{x|%Y-%m-%d}<br>Presence Score: %{y:.2f}<br><extra></extra>'
    ) %>%
    layout(xaxis = list(title = "Date", showgrid = FALSE), yaxis = list(title = "Presence Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
           hovermode = 'closest', plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
           margin = list(l = 50, r = 150, t = 30, b = 80),
           legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02), showlegend = TRUE
    ) %>% config(displayModeBar = TRUE)
})

output$query_chart_perception <- renderPlotly({
  req(query_timeseries(), query_top10_data())
  data <- query_timeseries()
  top_brands <- query_top10_data() %>% head(10) %>% pull(brand_name)
  data_filtered <- data %>% filter(brand_name %in% top_brands)
  
  plot_ly() %>%
    add_trace(data = data_filtered, x = ~date, y = ~perception_score, color = ~brand_name,
              type = 'scatter', mode = 'lines+markers', line = list(width = 2), marker = list(width = 2),
              hovertemplate = '<b>%{fullData.name}</b><br>Date: %{x|%Y-%m-%d}<br>Perception Score: %{y:.2f}<br><extra></extra>'
    ) %>%
    layout(xaxis = list(title = "Date", showgrid = FALSE), yaxis = list(title = "Perception Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
           hovermode = 'closest', plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
           margin = list(l = 50, r = 150, t = 30, b = 80),
           legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02), showlegend = TRUE
    ) %>% config(displayModeBar = TRUE)
})

output$query_chart_prestige <- renderPlotly({
  req(query_timeseries(), query_top10_data())
  data <- query_timeseries()
  top_brands <- query_top10_data() %>% head(10) %>% pull(brand_name)
  data_filtered <- data %>% filter(brand_name %in% top_brands)
  
  plot_ly() %>%
    add_trace(data = data_filtered, x = ~date, y = ~prestige_score, color = ~brand_name,
              type = 'scatter', mode = 'lines+markers', line = list(width = 2), marker = list(width = 2),
              hovertemplate = '<b>%{fullData.name}</b><br>Date: %{x|%Y-%m-%d}<br>Prestige Score: %{y:.2f}<br><extra></extra>'
    ) %>%
    layout(xaxis = list(title = "Date", showgrid = FALSE), yaxis = list(title = "Prestige Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
           hovermode = 'closest', plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
           margin = list(l = 50, r = 150, t = 30, b = 80),
           legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02), showlegend = TRUE
    ) %>% config(displayModeBar = TRUE)
})

output$query_chart_persistence <- renderPlotly({
  req(query_timeseries(), query_top10_data())
  data <- query_timeseries()
  top_brands <- query_top10_data() %>% head(10) %>% pull(brand_name)
  data_filtered <- data %>% filter(brand_name %in% top_brands)
  
  plot_ly() %>%
    add_trace(data = data_filtered, x = ~date, y = ~persistence_score, color = ~brand_name,
              type = 'scatter', mode = 'lines+markers', line = list(width = 2), marker = list(width = 2),
              hovertemplate = '<b>%{fullData.name}</b><br>Date: %{x|%Y-%m-%d}<br>Persistence Score: %{y:.2f}<br><extra></extra>'
    ) %>%
    layout(xaxis = list(title = "Date", showgrid = FALSE), yaxis = list(title = "Persistence Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
           hovermode = 'closest', plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
           margin = list(l = 50, r = 150, t = 30, b = 80),
           legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02), showlegend = TRUE
    ) %>% config(displayModeBar = TRUE)
})