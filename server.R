source("global.R")
source("custom_css_stuff.R")

server <- function(input, output, session) {
  
  # Reactive values-------------
  rv <- reactiveValues(
    logged_in = FALSE,
    customer_id = NULL,
    customer_name = NULL,
    email = NULL,
    auth_message = NULL,
    auth_type = NULL,
    queries_refresh = 0
  )
  
  # Authentication title
  output$auth_title <- renderText({
    if (is.null(input$show_register) || !input$show_register) {
      "Welcome Back"
    } else {
      "Create Account"
    }
  })
  
  output$auth_subtitle <- renderText({
    if (is.null(input$show_register) || !input$show_register) {
      "Sign in to access your dashboard"
    } else {
      "Join us and start tracking your progress"
    }
  })
  
  # Login status output
  output$logged_in <- reactive({
    rv$logged_in
  })
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  # Authentication alert messages
  output$auth_alert <- renderUI({
    if (!is.null(rv$auth_message)) {
      div(
        class = paste0("alert alert-", rv$auth_type),
        rv$auth_message
      )
    }
  })
  
  # Login button handler
  observeEvent(input$login_btn, {
    customer_name <- trimws(input$customer_name)
    password <- input$login_password
    
    if (customer_name == "" || password == "") {
      rv$auth_message <- "Please enter both username and password"
      rv$auth_type <- "danger"
      return()
    }
    
    customer <- verify_user(customer_name, password)
    
    if (!is.null(customer)) {
      rv$logged_in <- TRUE
      rv$customer_id <- customer$customer_id
      rv$customer_name <- customer$customer_name
      rv$email <- customer$email
      rv$auth_message <- NULL
      
      showNotification(
        "Login successful!",
        type = "message",
        duration = 3
      )
    } else {
      rv$auth_message <- "Invalid username or password"
      rv$auth_type <- "danger"
    }
  })
  
  # Register button handler
  observeEvent(input$register_btn, {
    customer_name <- trimws(input$customer_name)
    email <- trimws(input$register_email)
    password <- input$register_password
    password_confirm <- input$register_password_confirm
    
    # Validation
    if (customer_name == "" || email == "" || password == "" || password_confirm == "") {
      rv$auth_message <- "Please fill in all fields"
      rv$auth_type <- "danger"
      return()
    }
    
    if (password != password_confirm) {
      rv$auth_message <- "Passwords do not match"
      rv$auth_type <- "danger"
      return()
    }
    
    if (nchar(password) < 6) {
      rv$auth_message <- "Password must be at least 6 characters"
      rv$auth_type <- "danger"
      return()
    }
    
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$", email)) {
      rv$auth_message <- "Please enter a valid email address"
      rv$auth_type <- "danger"
      return()
    }
    
    # Create user
    customer_id <- create_user(customer_name, email, password)
    
    if (!is.null(customer_id)) {
      rv$logged_in <- TRUE
      rv$customer_id <- customer_id
      rv$customer_name <- customer_name
      rv$email <- email
      rv$auth_message <- NULL
      
      showNotification(
        "Account created successfully!",
        type = "message",
        duration = 3
      )
    } else {
      rv$auth_message <- "Username or email already exists"
      rv$auth_type <- "danger"
    }
  })
  
  # Logout handler
  observeEvent(input$logout, {
    rv$logged_in <- FALSE
    rv$customer_id <- NULL
    rv$customer_name <- NULL
    rv$email <- NULL
    rv$auth_message <- NULL
    
    showNotification(
      "Logged out successfully",
      type = "message",
      duration = 3
    )
  })
  
  # User info display
  output$user_info <- renderUI({
    if (rv$logged_in) {
      tags$span(
        style = "color: white; padding-right: 10px; font-weight: 500;",
        icon("user-circle"),
        " ",
        rv$customer_name
      )
    }
  })
  
  # Get user data
  user_metrics <- reactive({
    req(rv$logged_in, rv$customer_id)
    get_user_metrics(rv$customer_id)
  })
  
  # user_timeseries <- reactive({
  #   req(rv$logged_in, rv$customer_id)
  #   get_user_timeseries(rv$customer_id)
  # })
  
  # Value boxes
  output$metric_box_1 <- renderValueBox({
    req(user_metrics())
    metrics <- user_metrics()
    
    airr_score <- get_latest_score(metrics$airr, "airr_score")
    
    valueBox(
      value = format(round(airr_score, 1), big.mark = ","),
      subtitle = "AiRR Score",
      icon = icon("trophy"),
      color = "aqua"
    )
  })
  
  output$metric_box_2 <- renderValueBox({
    req(user_metrics())
    
    # Calculate average
    avg <- mean(user_metrics()$metric_value, na.rm = TRUE)
    
    valueBox(
      value = format(round(avg, 1), nsmall = 1),
      subtitle = "Average Performance",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # output$metric_box_3 <- renderValueBox({
  #   req(user_timeseries())
  #   
  #   # Count data points
  #   count <- nrow(user_timeseries())
  #   
  #   valueBox(
  #     value = count,
  #     subtitle = "Data Points",
  #     icon = icon("database"),
  #     color = "yellow"
  #   )
  # })
  
  output$metric_box_4 <- renderValueBox({
    req(user_metrics())
    
    # Count categories
    categories <- length(unique(user_metrics()$category))
    
    valueBox(
      value = categories,
      subtitle = "Active Categories",
      icon = icon("layer-group"),
      color = "red"
    )
  })
  
  # Time series chart
  output$timeseries_chart <- renderPlotly({
    req(user_metrics())
    
    metrics <- user_metrics()
    
    data <- metrics$persistence %>%
      arrange(date)
    
    # Calculate y-axis range
    min_score <- min(data$persistence_score, na.rm = TRUE)
    y_min <- max(0, min_score - 20)  # Either 0 or 20 below lowest, whichever is higher
    y_max <- 105
    
    # Create multiple traces for different metrics
    plot_ly() %>%
      add_trace(
        data = data,
        x = ~date,
        y = ~persistence_score,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 3),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Persistence Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        title = list(
          text = "",
          font = list(size = 16, color = app_colors$dark)
        ),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          gridcolor = "#E8EDF2"
        ),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          gridcolor = "#E8EDF2",
          range = c(y_min, y_max)
        ),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 50, t = 30, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Spider/Radar chart
  output$spider_chart <- renderPlotly({
    req(user_metrics())
    
    metrics <- user_metrics()
    
    # Create spider chart data
    # Replace these column names with your actual column names
    spider_data <- data.frame(
      category = c("Presence", "Perception", "Prestige", "Persistence"),
      value = c(
        get_latest_score(metrics$presence, "overall_score"),    # adjust column name
        get_latest_score(metrics$perception, "perception_score"), # adjust column name
        get_latest_score(metrics$prestige, "prestige_score"),     # adjust column name
        get_latest_score(metrics$persistence, "persistence_score") # adjust column name
      )
    )
    
    plot_ly(
      type = 'scatterpolar',
      r = spider_data$value,
      theta = spider_data$category,
      fill = 'toself',
      fillcolor = 'rgba(102, 126, 234, 0.5)',
      line = list(
        color = '#667eea',
        width = 2
      ),
      marker = list(
        size = 3,
        color = '#667eea'
      ),
      hovertemplate = paste(
        '<b>%{theta}</b><br>',
        'Score: %{r:.1f}/100<br>',
        'Actual: ', round(spider_data$value, 1), '<br>',
        '<extra></extra>'
      )
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            showgrid = TRUE,
            gridcolor = "#E8EDF2"
          ),
          angularaxis = list(
            showgrid = TRUE,
            gridcolor = "#E8EDF2"
          ),
          bgcolor = "#FFFFFF"
        ),
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 80, r = 80, t = 30, b = 30),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Activity table
  output$activity_table <- DT::renderDataTable({
    req(user_metrics())
    
    user_metrics() %>%
      mutate(
        recorded_at = format(recorded_at, "%Y-%m-%d %H:%M")
      ) %>%
      select(
        Date = recorded_at,
        Metric = metric_name,
        Category = category,
        Value = metric_value
      ) %>%
      arrange(desc(Date)) %>%
      head(10)
  }, options = list(
    pageLength = 10,
    dom = 't',
    ordering = TRUE,
    columnDefs = list(
      list(className = 'dt-center', targets = '_all')
    )
  ), rownames = FALSE)
  
  observeEvent(input$submit_prompt_btn, {
    req(input$prompt_input)
    req(user_metrics())
    
    metrics <- user_metrics()
    # Call your function
    addPrompt(input$prompt_input, metrics$cust$customer_id)
    
    rv$queries_refresh <- rv$queries_refresh + 1
    
    # Clear the input
    updateTextInput(session, "prompt_input", value = "")
    
    # Optional: Show notification
    showNotification(
      "Prompt submitted successfully!",
      type = "message",
      duration = 3
    )
  })
  
  # Get customer queries reactive --------
  customer_queries <- reactive({
    req(rv$logged_in, rv$customer_id)
    
    # This will invalidate whenever queries_refresh changes
    rv$queries_refresh
    
    get_customer_queries(rv$customer_id)
  })
  
  # Queries summary
  output$queries_summary <- renderUI({
    req(customer_queries())
    req(user_metrics())
    
    queries <- customer_queries()
    customer_id <- metrics$cust$customer_id
    
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
    req(customer_queries())
    req(user_metrics())
    
    queries <- customer_queries()
    customer_id <- metrics$cust$customer_id
    
    if (nrow(queries) == 0) {
      return(NULL)
    }
    
    # Create a panel for each query
    query_panels <- lapply(1:nrow(queries), function(i) {
      query_id <- queries$query_id[i]
      query_string <- queries$query_string[i]
      date_added <- queries$date_added[i]
      
      # Create unique IDs for this query's outputs
      valuebox_id <- paste0("query_valuebox_", query_id)
      spider_id <- paste0("query_spider_", query_id)
      timeseries_id <- paste0("query_timeseries_", query_id)
      
      # Create the panel
      fluidRow(
        box(
          title = div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span(
              strong(paste0("Query ", i,": ")),
              query_string
            ),
            span(
              style = "font-size: 12px; color: #7f8c8d; font-weight: normal;",
              paste("Added:", format(date_added, "%b %d, %Y"))
            )
          ),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = (i > 1),  # Collapse all except first
          
          # fluidRow(
          #   column(
          #     width = 12,
          #     valueBoxOutput(valuebox_id, width = 12)
          #   )
          # ),
          
          fluidRow(
            column(
              width = 3,
              valueBoxOutput(valuebox_id, width = 12)
            ),
            column(
              width = 5,
              box(
                title = "AiRR Score Over Time",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                shinycssloaders::withSpinner(
                  plotlyOutput(timeseries_id, height = "350px"),
                  type = 4,
                  color = "#667eea"
                )
              )
            ),
            column(
              width = 4,
              box(
                title = "Four P's Breakdown",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                shinycssloaders::withSpinner(
                  plotlyOutput(spider_id, height = "350px"),
                  type = 4,
                  color = "#667eea"
                )
              )
            )
          )
        )
      )
    })
    
    # Return all panels
    do.call(tagList, query_panels)
  })
  
  # Observe customer queries and create outputs dynamically
  observe({
    
    req(customer_queries())
    req(user_metrics())
    
    queries <- customer_queries()
    customer_id <- metrics$cust$customer_id
    
    if (nrow(queries) == 0) {
      return()
    }
    
    # Create outputs for each query
    lapply(1:nrow(queries), function(i) {
      query_id <- queries$query_id[i]
      
      # Value Box
      output[[paste0("query_valuebox_", query_id)]] <- renderValueBox({
        latest_scores <- get_latest_query_scores(query_id, customer_id)
        
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
        latest_scores <- get_latest_query_scores(query_id, customer_id)
        
        if (is.null(latest_scores$airr_score) || latest_scores$airr_score == 0) {
          # Return empty plot with message
          plot_ly() %>%
            layout(
              annotations = list(
                text = "No data available",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "#999")
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        } else {
          spider_data <- data.frame(
            category = c("Presence", "Perception", "Prestige", "Persistence"),
            value = c(
              latest_scores$presence_score,
              latest_scores$perception_score,
              latest_scores$prestige_score,
              latest_scores$persistence_score
            )
          )
          
          # Ensure values are 0-100
          spider_data$normalized <- pmin(100, pmax(0, spider_data$value))
          
          plot_ly(
            type = 'scatterpolar',
            r = spider_data$normalized,
            theta = spider_data$category,
            fill = 'toself',
            fillcolor = 'rgba(102, 126, 234, 0.5)',
            line = list(
              color = '#667eea',
              width = 3
            ),
            marker = list(
              size = 10,
              color = '#667eea'
            ),
            hovertemplate = paste(
              '<b>%{theta}</b><br>',
              'Score: %{r:.1f}/100<br>',
              '<extra></extra>'
            )
          ) %>%
            layout(
              polar = list(
                radialaxis = list(
                  visible = TRUE,
                  range = c(0, 100),
                  showgrid = TRUE,
                  gridcolor = "#E8EDF2",
                  tickfont = list(size = 10)
                ),
                angularaxis = list(
                  showgrid = TRUE,
                  gridcolor = "#E8EDF2",
                  tickfont = list(size = 11, family = "Poppins")
                ),
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
          # Return empty plot with message
          plot_ly() %>%
            layout(
              annotations = list(
                text = "No historical data available",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "#999")
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        } else {
          # Calculate y-axis range
          min_score <- min(history$airr_score, na.rm = TRUE)
          y_min <- max(0, min_score - 20)
          y_max <- 100
          
          plot_ly() %>%
            add_trace(
              data = history,
              x = ~date,
              y = ~airr_score,
              type = 'scatter',
              mode = 'lines+markers',
              name = 'AiRR Score',
              line = list(
                width = 3,
                color = '#667eea'
              ),
              marker = list(
                size = 8,
                color = '#667eea'
              ),
              hovertemplate = paste(
                '<b>AiRR Score</b><br>',
                'Date: %{x|%Y-%m-%d}<br>',
                'Score: %{y:.2f}<br>',
                '<extra></extra>'
              )
            ) %>%
            layout(
              title = list(
                text = "",
                font = list(size = 16, color = app_colors$dark)
              ),
              xaxis = list(
                title = "Date",
                showgrid = TRUE,
                gridcolor = "#E8EDF2"
              ),
              yaxis = list(
                title = "AiRR Score",
                showgrid = TRUE,
                gridcolor = "#E8EDF2",
                range = c(y_min, y_max)
              ),
              hovermode = 'closest',
              plot_bgcolor = "#FFFFFF",
              paper_bgcolor = "#FFFFFF",
              margin = list(l = 50, r = 50, t = 30, b = 50)
            ) %>%
            config(displayModeBar = FALSE)
        }
      })
    })
  })
  
  
  # -----------
  
  # Detailed spider chart
  output$detailed_spider <- renderPlotly({
    req(user_metrics())
    
    metrics_by_cat <- user_metrics() %>%
      group_by(category) %>%
      summarise(
        value = mean(metric_value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    metrics_by_cat$normalized <- scales::rescale(metrics_by_cat$value, to = c(0, 100))
    
    plot_ly(
      type = 'scatterpolar',
      r = metrics_by_cat$normalized,
      theta = metrics_by_cat$category,
      fill = 'toself',
      fillcolor = 'rgba(102, 126, 234, 0.3)',
      line = list(
        color = app_colors$secondary,
        width = 4
      ),
      marker = list(
        size = 10,
        color = app_colors$secondary
      ),
      hovertemplate = paste(
        '<b>%{theta}</b><br>',
        'Score: %{r:.1f}/100<br>',
        '<extra></extra>'
      )
    ) %>%
      layout(
        title = list(
          text = "Performance Radar",
          font = list(size = 18, color = app_colors$dark)
        ),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            showgrid = TRUE,
            gridcolor = "#E8EDF2"
          ),
          angularaxis = list(
            showgrid = TRUE,
            gridcolor = "#E8EDF2"
          ),
          bgcolor = "#FFFFFF"
        ),
        paper_bgcolor = "#FFFFFF",
        showlegend = FALSE,
        margin = list(l = 100, r = 100, t = 80, b = 50)
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Trend analysis
  output$trend_chart <- renderPlotly({
    req(user_timeseries())
    
    # Calculate moving average
    data <- user_timeseries() %>%
      arrange(metric_name, recorded_at) %>%
      group_by(metric_name) %>%
      mutate(
        ma_7 = zoo::rollmean(metric_value, k = 7, fill = NA, align = "right")
      ) %>%
      ungroup()
    
    plot_ly() %>%
      add_trace(
        data = data,
        x = ~recorded_at,
        y = ~metric_value,
        color = ~metric_name,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 6, opacity = 0.5),
        name = ~paste(metric_name, "(actual)"),
        showlegend = TRUE,
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Value: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      add_trace(
        data = data,
        x = ~recorded_at,
        y = ~ma_7,
        color = ~metric_name,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 3),
        name = ~paste(metric_name, "(trend)"),
        showlegend = TRUE,
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          '7-day avg: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        title = list(
          text = "Trend Analysis (7-day Moving Average)",
          font = list(size = 18, color = app_colors$dark)
        ),
        xaxis = list(
          title = "Date",
          showgrid = TRUE,
          gridcolor = "#E8EDF2"
        ),
        yaxis = list(
          title = "Value",
          showgrid = TRUE,
          gridcolor = "#E8EDF2"
        ),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        legend = list(
          orientation = "h",
          yanchor = "bottom",
          y = -0.4,
          xanchor = "center",
          x = 0.5
        ),
        margin = list(l = 60, r = 50, t = 60, b = 120)
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Profile information
  output$profile_info <- renderUI({
    req(rv$logged_in)
    
    tagList(
      div(
        style = "text-align: center; padding: 20px;",
        icon("user-circle", class = "fa-5x", style = "color: #667eea;"),
        h3(style = "margin-top: 20px;", rv$customer_name),
        p(style = "color: #7f8c8d;", rv$email)
      ),
      hr(),
      div(
        style = "padding: 15px;",
        h4("Account Details"),
        p(strong("User ID: "), rv$customer_id),
        p(strong("Username: "), rv$customer_name),
        p(strong("Email: "), rv$email),
        p(strong("Member Since: "), format(Sys.Date(), "%B %Y"))
      )
    )
  })
  
  # Account statistics
  output$account_stats <- renderUI({
    req(user_metrics(), user_timeseries())
    
    total_metrics <- nrow(user_metrics())
    total_records <- nrow(user_timeseries())
    categories <- length(unique(user_metrics()$category))
    
    tagList(
      div(
        style = "padding: 20px;",
        div(
          style = "margin-bottom: 30px;",
          h4(style = "color: #667eea; font-size: 36px; margin: 0;", total_metrics),
          p(style = "color: #7f8c8d; margin: 5px 0;", "Total Metrics")
        ),
        div(
          style = "margin-bottom: 30px;",
          h4(style = "color: #27AE60; font-size: 36px; margin: 0;", total_records),
          p(style = "color: #7f8c8d; margin: 5px 0;", "Total Records")
        ),
        div(
          style = "margin-bottom: 30px;",
          h4(style = "color: #F39C12; font-size: 36px; margin: 0;", categories),
          p(style = "color: #7f8c8d; margin: 5px 0;", "Categories")
        ),
        div(
          h4(style = "color: #E74C3C; font-size: 36px; margin: 0;", 
             ifelse(nrow(user_timeseries()) > 0,
                    as.numeric(difftime(Sys.Date(), 
                                        min(user_timeseries()$recorded_at), 
                                        units = "days")),
                    0)),
          p(style = "color: #7f8c8d; margin: 5px 0;", "Days Active")
        )
      )
    )
  })
  
}