source("global.R")
source("custom_css_stuff.R")

server <- function(input, output, session) {
  
  # Force error logging
  # options(shiny.error = browser)
  # options(shiny.fullstacktrace = TRUE)
  
  # Log file in app directory
  # logfile <- "/srv/shiny-server/AiRR/app_debug.log"
  
  # Log everything
  # log_message <- function(msg) {
  #   cat(paste0(Sys.time(), ": ", msg, "\n"), 
  #       file = logfile, append = TRUE)
  # }
  
  # log_message("App started")
  
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
    # log_message("Login button clicked")
    customer_name <- trimws(input$customer_name)
    password <- input$login_password
    
    if (customer_name == "" || password == "") {
      rv$auth_message <- "Please enter both username and password"
      rv$auth_type <- "danger"
      return()
    }
    
    if(customer_name == 'Demo' && password == 'Demo'){
      rv$logged_in <- TRUE
      rv$customer_id <- 0
      rv$customer_name <- "Demo"
      rv$email <- "demo@airr.com"
      rv$auth_message <- NULL
      
      showNotification(
        "Login successful!",
        type = "message",
        duration = 3
      )
    } else {
      customer <- verify_user(customer_name, password)
      
      if (!is.null(customer)) {
        # log_message("Login processing...")
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
        
        # log_message("Login successful")
      } else {
        rv$auth_message <- "Invalid username or password"
        rv$auth_type <- "danger"
      }
    }
  })
  
  # Register button handler
  observeEvent(input$register_btn, {
    customer_name <- trimws(input$register_customer_name)
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
    
    # if (nchar(password) < 6) {
    #   rv$auth_message <- "Password must be at least 6 characters"
    #   rv$auth_type <- "danger"
    #   return()
    # }
    
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
      
      # auto generate airr score on user create
      user_create_airr(customer_name)
      
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
    
    # log_message("user info output creation")
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
      arrange(date) %>%
      left_join(metrics$presence %>% select(date, overall_score) %>%
                  rename('presence_score' = 'overall_score'),
                by = 'date') %>%
      left_join(metrics$perception %>% select(date, perception_score),
                by = 'date') %>%
      left_join(metrics$prestige %>% select(date, prestige_score),
                by = 'date') %>%
      left_join(metrics$airr %>% select(date, airr_score),
                by = 'date')
    
    # Calculate y-axis range
    min_score <- min(c(data$presence_score, data$perception_score, data$prestige_score, data$persistence_score, data$airr_score), na.rm = TRUE)
    y_min <- max(0, min_score - 20)  # Either 0 or 20 below lowest, whichever is higher
    y_max <- 105
    
    # Create multiple traces for different metrics
    plot_ly() %>%
      # Main line - airr_score
      add_trace(
        data = data,
        x = ~date,
        y = ~airr_score,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'AIRR Score',
        line = list(width = 3),
        marker = list(size = 3),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      # Presence Score
      add_trace(
        data = data,
        x = ~date,
        y = ~presence_score,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Presence Score',
        line = list(width = 1),
        marker = list(size = 1),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      # Perception Score
      add_trace(
        data = data,
        x = ~date,
        y = ~perception_score,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Perception Score',
        line = list(width = 1),
        marker = list(size = 1),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      # Prestige Score
      add_trace(
        data = data,
        x = ~date,
        y = ~prestige_score,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Prestige Score',
        line = list(width = 1),
        marker = list(size = 1),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      # Persistence Score
      add_trace(
        data = data,
        x = ~date,
        y = ~persistence_score,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Persistence Score',
        line = list(width = 1),
        marker = list(size = 1),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
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
        margin = list(l = 50, r = 50, t = 30, b = 80),
        legend = list(
          orientation = "h",
          yanchor = "bottom",
          y = -0.2,
          xanchor = "center",
          x = 0.5
        )
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
  
  competitor_list <- reactive({
    req(user_metrics())
    
    metrics <- user_metrics()
    
    # Query all customers from database
    all_customers <- dbGetQuery(
      con,  # Your postgres connection object
      "SELECT DISTINCT customer_name FROM dim_customer ORDER BY customer_name"
    )
    
    # Remove the current customer from the list
    filtered_customers <- all_customers$customer_name[
      all_customers$customer_name != metrics$cust$customer_name
    ]
    
    return(filtered_customers)
  })
  
  observe({
    updateSelectInput(
      session,
      "competitor_select",
      choices = competitor_list(),
      selected = character(0)
    )
  })
  
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
    
    metrics <- user_metrics()
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
    
    metrics <- user_metrics()
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
    
    metrics <- user_metrics()
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
  
  
  leaderboard_data <- reactive({
    # Get most recent AIRR score for each customer
    query <- "
    WITH latest_dates AS (
      SELECT 
        customer_id,
        MAX(date) as latest_date
      FROM fact_airr_history
      WHERE airr_score IS NOT NULL
      GROUP BY customer_id
    )
    SELECT 
      dc.customer_name,
      fa.airr_score,
      fpres.overall_score as presence_score,
      fperc.perception_score,
      fprest.prestige_score,
      fpers.persistence_score,
      fa.date
    FROM dim_customer dc
    INNER JOIN latest_dates ld ON dc.customer_id = ld.customer_id
    INNER JOIN fact_airr_history fa ON dc.customer_id = fa.customer_id 
      AND fa.date = ld.latest_date
    LEFT JOIN fact_presence_history fpres ON dc.customer_id = fpres.customer_id 
      AND fpres.date = ld.latest_date
    LEFT JOIN fact_perception_history fperc ON dc.customer_id = fperc.customer_id 
      AND fperc.date = ld.latest_date
    LEFT JOIN fact_prestige_history fprest ON dc.customer_id = fprest.customer_id 
      AND fprest.date = ld.latest_date
    LEFT JOIN fact_persistence_history fpers ON dc.customer_id = fpers.customer_id 
      AND fpers.date = ld.latest_date
    ORDER BY fa.airr_score DESC
  "
    
    dbGetQuery(con, query)
  })
  
  output$leaderboard_table <- renderDT({
    req(leaderboard_data())
    
    datatable(
      leaderboard_data(),
      # ttt,
      options = list(
        pageLength = 25,
        order = list(list(1, 'desc')),  # Sort by AIRR Score descending
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatRound(c('airr_score','presence_score','perception_score',
                    'prestige_score','persistence_score'), 2) %>%
      formatDate('date', 'toDateString')
  })
  
  
  leaderboard_timeseries <- reactive({
    query <- "
    SELECT 
      dc.customer_name,
      fa.date,
      fa.airr_score
    FROM dim_customer dc
    LEFT JOIN fact_airr_history fa ON dc.customer_id = fa.customer_id
    WHERE fa.airr_score IS NOT NULL
    ORDER BY dc.customer_name, fa.date
  "
    
    dbGetQuery(con, query)
  })
  
  
  
  # AIRR Score Chart
  output$leaderboard_chart_airr <- renderPlotly({
    req(leaderboard_timeseries())

    data <- leaderboard_timeseries()

    # Get top 10 customers by most recent score
    top_customers <- leaderboard_data() %>%
      head(10) %>%
      pull(customer_name)

    data_filtered <- data %>%
      filter(customer_name %in% top_customers)

    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~airr_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(size = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'AIRR Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(
          title = "Date",
          showgrid = FALSE,
          gridcolor = "#E8EDF2"
        ),
        yaxis = list(
          title = "AIRR Score",
          showgrid = TRUE,
          gridcolor = "#E8EDF2"
        ),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(
          orientation = "v",
          yanchor = "top",
          y = 1,
          xanchor = "left",
          x = 1.02
        ),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })

  # Presence Score Chart
  output$leaderboard_chart_presence <- renderPlotly({
    req(leaderboard_timeseries())

    # Get presence data
    data <- dbGetQuery(con, "
    SELECT
      dc.customer_name,
      fph.date,
      fph.overall_score as presence_score
    FROM dim_customer dc
    LEFT JOIN fact_presence_history fph ON dc.customer_id = fph.customer_id
    WHERE fph.overall_score IS NOT NULL
    ORDER BY dc.customer_name, fph.date
  ")

    # Get top 10 customers
    top_customers <- leaderboard_data() %>%
      head(10) %>%
      pull(customer_name)

    data_filtered <- data %>%
      filter(customer_name %in% top_customers)

    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~presence_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(size = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Presence Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Presence Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })

  # Perception Score Chart
  output$leaderboard_chart_perception <- renderPlotly({
    req(leaderboard_timeseries())

    data <- dbGetQuery(con, "
    SELECT
      dc.customer_name,
      fph.date,
      fph.perception_score
    FROM dim_customer dc
    LEFT JOIN fact_perception_history fph ON dc.customer_id = fph.customer_id
    WHERE fph.perception_score IS NOT NULL
    ORDER BY dc.customer_name, fph.date
  ")

    top_customers <- leaderboard_data() %>%
      head(10) %>%
      pull(customer_name)

    data_filtered <- data %>%
      filter(customer_name %in% top_customers)

    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~perception_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(size = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Perception Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Perception Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })

  # Prestige Score Chart
  output$leaderboard_chart_prestige <- renderPlotly({
    req(leaderboard_timeseries())

    data <- dbGetQuery(con, "
    SELECT
      dc.customer_name,
      fph.date,
      fph.prestige_score
    FROM dim_customer dc
    LEFT JOIN fact_prestige_history fph ON dc.customer_id = fph.customer_id
    WHERE fph.prestige_score IS NOT NULL
    ORDER BY dc.customer_name, fph.date
  ")

    top_customers <- leaderboard_data() %>%
      head(10) %>%
      pull(customer_name)

    data_filtered <- data %>%
      filter(customer_name %in% top_customers)

    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~prestige_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(size = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Prestige Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Prestige Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })

  # Persistence Score Chart
  output$leaderboard_chart_persistence <- renderPlotly({
    req(leaderboard_timeseries())

    data <- dbGetQuery(con, "
    SELECT
      dc.customer_name,
      fph.date,
      fph.persistence_score
    FROM dim_customer dc
    LEFT JOIN fact_persistence_history fph ON dc.customer_id = fph.customer_id
    WHERE fph.persistence_score IS NOT NULL
    ORDER BY dc.customer_name, fph.date
  ")

    top_customers <- leaderboard_data() %>%
      head(10) %>%
      pull(customer_name)

    data_filtered <- data %>%
      filter(customer_name %in% top_customers)

    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~persistence_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(size = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Persistence Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Persistence Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  
  query_timeseries <- reactive({
    req(input$query_select)
    req(input$query_select != "")
    
    query_selected <- input$query_select
    
    query <- "
    SELECT 
      dc.customer_name,
      fqh.date,
      fqh.airr_score,
      fqh.presence_score,
      fqh.perception_score,
      fqh.prestige_score,
      fqh.persistence_score
    FROM fact_query_history fqh
    INNER JOIN dim_customer dc ON fqh.customer_id = dc.customer_id
    INNER JOIN dim_query dq ON fqh.query_id = dq.query_id
    WHERE dq.query_string = $1
      AND fqh.airr_score IS NOT NULL
    ORDER BY dc.customer_name, fqh.date
  "
    
    dbGetQuery(con, query, params = list(query_selected))
  })
  
  # AIRR Score Chart for Query
  output$query_chart_airr <- renderPlotly({
    req(query_timeseries())
    req(query_top10_data())
    
    data <- query_timeseries()
    
    # Get top 10 customers
    top_customers <- query_top10_data() %>%
      head(10) %>%
      pull(customer_name)
    
    data_filtered <- data %>%
      filter(customer_name %in% top_customers)
    
    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~airr_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(width = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'AIRR Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(
          title = "Date",
          showgrid = FALSE,
          gridcolor = "#E8EDF2"
        ),
        yaxis = list(
          title = "AIRR Score",
          showgrid = TRUE,
          gridcolor = "#E8EDF2"
        ),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(
          orientation = "v",
          yanchor = "top",
          y = 1,
          xanchor = "left",
          x = 1.02
        ),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Presence Score Chart for Query
  output$query_chart_presence <- renderPlotly({
    req(query_timeseries())
    req(query_top10_data())
    
    data <- query_timeseries()
    
    top_customers <- query_top10_data() %>%
      head(10) %>%
      pull(customer_name)
    
    data_filtered <- data %>%
      filter(customer_name %in% top_customers)
    
    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~presence_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(width = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Presence Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Presence Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Perception Score Chart for Query
  output$query_chart_perception <- renderPlotly({
    req(query_timeseries())
    req(query_top10_data())
    
    data <- query_timeseries()
    
    top_customers <- query_top10_data() %>%
      head(10) %>%
      pull(customer_name)
    
    data_filtered <- data %>%
      filter(customer_name %in% top_customers)
    
    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~perception_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(width = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Perception Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Perception Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Prestige Score Chart for Query
  output$query_chart_prestige <- renderPlotly({
    req(query_timeseries())
    req(query_top10_data())
    
    data <- query_timeseries()
    
    top_customers <- query_top10_data() %>%
      head(10) %>%
      pull(customer_name)
    
    data_filtered <- data %>%
      filter(customer_name %in% top_customers)
    
    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~prestige_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(width = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Prestige Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Prestige Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # Persistence Score Chart for Query
  output$query_chart_persistence <- renderPlotly({
    req(query_timeseries())
    req(query_top10_data())
    
    data <- query_timeseries()
    
    top_customers <- query_top10_data() %>%
      head(10) %>%
      pull(customer_name)
    
    data_filtered <- data %>%
      filter(customer_name %in% top_customers)
    
    plot_ly() %>%
      add_trace(
        data = data_filtered,
        x = ~date,
        y = ~persistence_score,
        color = ~customer_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 2),
        markers = list(width = 2),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Persistence Score: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        xaxis = list(title = "Date", showgrid = FALSE, gridcolor = "#E8EDF2"),
        yaxis = list(title = "Persistence Score", showgrid = TRUE, gridcolor = "#E8EDF2"),
        hovermode = 'closest',
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 50, r = 150, t = 30, b = 80),
        legend = list(orientation = "v", yanchor = "top", y = 1, xanchor = "left", x = 1.02),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  
  
  
  
  
  # Populate customer dropdowns
  observe({
    all_customers <- dbGetQuery(
      con,
      "SELECT DISTINCT customer_name FROM dim_customer ORDER BY customer_name"
    )$customer_name
    
    updateSelectInput(session, "compare_customer_1", choices = c("Select...", all_customers))
    updateSelectInput(session, "compare_customer_2", choices = c("Select...", all_customers))
    updateSelectInput(session, "compare_customer_3", choices = c("Select...", all_customers))
  })
  
  # Get comparison data for selected customers
  comparison_data <- reactive({
    # Get selected customers (excluding "Select...")
    selected_customers <- c(
      input$compare_customer_1,
      input$compare_customer_2,
      input$compare_customer_3
    )
    selected_customers <- selected_customers[selected_customers != "Select..." & !is.null(selected_customers)]
    
    req(length(selected_customers) > 0)
    
    # Create parameterized query
    placeholders <- paste0("$", 1:length(selected_customers), collapse = ", ")
    
    query <- sprintf("
  WITH latest_presence AS (
    SELECT 
      dc.customer_name,
      fph.overall_score as presence_score,
      ROW_NUMBER() OVER (PARTITION BY dc.customer_id ORDER BY fph.date DESC) as rn
    FROM dim_customer dc
    LEFT JOIN fact_presence_history fph ON dc.customer_id = fph.customer_id
    WHERE dc.customer_name IN (%s)
      AND fph.overall_score IS NOT NULL
  ),
  latest_perception AS (
    SELECT 
      dc.customer_name,
      fph.perception_score,
      ROW_NUMBER() OVER (PARTITION BY dc.customer_id ORDER BY fph.date DESC) as rn
    FROM dim_customer dc
    LEFT JOIN fact_perception_history fph ON dc.customer_id = fph.customer_id
    WHERE dc.customer_name IN (%s)
      AND fph.perception_score IS NOT NULL
  ),
  latest_prestige AS (
    SELECT 
      dc.customer_name,
      fph.prestige_score,
      ROW_NUMBER() OVER (PARTITION BY dc.customer_id ORDER BY fph.date DESC) as rn
    FROM dim_customer dc
    LEFT JOIN fact_prestige_history fph ON dc.customer_id = fph.customer_id
    WHERE dc.customer_name IN (%s)
      AND fph.prestige_score IS NOT NULL
  ),
  latest_persistence AS (
    SELECT 
      dc.customer_name,
      fph.persistence_score,
      ROW_NUMBER() OVER (PARTITION BY dc.customer_id ORDER BY fph.date DESC) as rn
    FROM dim_customer dc
    LEFT JOIN fact_persistence_history fph ON dc.customer_id = fph.customer_id
    WHERE dc.customer_name IN (%s)
      AND fph.persistence_score IS NOT NULL
  )
  SELECT 
    COALESCE(pres.customer_name, perc.customer_name, prest.customer_name, pers.customer_name) as customer_name,
    COALESCE(pres.presence_score, 0) as presence_score,
    COALESCE(perc.perception_score, 0) as perception_score,
    COALESCE(prest.prestige_score, 0) as prestige_score,
    COALESCE(pers.persistence_score, 0) as persistence_score
  FROM latest_presence pres
  FULL OUTER JOIN latest_perception perc ON pres.customer_name = perc.customer_name AND perc.rn = 1
  FULL OUTER JOIN latest_prestige prest ON COALESCE(pres.customer_name, perc.customer_name) = prest.customer_name AND prest.rn = 1
  FULL OUTER JOIN latest_persistence pers ON COALESCE(pres.customer_name, perc.customer_name, prest.customer_name) = pers.customer_name AND pers.rn = 1
  WHERE pres.rn = 1
", placeholders, placeholders, placeholders, placeholders)
    
    # Execute with parameters - flatten the list
    result <- dbGetQuery(con, query, params = as.list(rep(selected_customers), 4))
    
    return(result)
  })
  
  # Render spider chart
  output$spider_chart_compare <- renderPlotly({
    req(comparison_data())
    
    data <- comparison_data()
    
    # Create spider/radar chart
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = if(nrow(data) >= 1) c(
          data$presence_score[1],
          data$perception_score[1],
          data$prestige_score[1],
          data$persistence_score[1],
          data$presence_score[1]  # Close the polygon
        ) else NULL,
        theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
        name = if(nrow(data) >= 1) data$customer_name[1] else NULL,
        line = list(width = 3),
        marker = list(size = 8),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          '%{theta}: %{r:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      {
        if(nrow(data) >= 2) {
          add_trace(
            .,
            r = c(
              data$presence_score[2],
              data$perception_score[2],
              data$prestige_score[2],
              data$persistence_score[2],
              data$presence_score[2]
            ),
            theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
            name = data$customer_name[2],
            line = list(width = 3),
            marker = list(size = 8),
            hovertemplate = paste(
              '<b>%{fullData.name}</b><br>',
              '%{theta}: %{r:.2f}<br>',
              '<extra></extra>'
            )
          )
        } else {
          .
        }
      } %>%
      {
        if(nrow(data) >= 3) {
          add_trace(
            .,
            r = c(
              data$presence_score[3],
              data$perception_score[3],
              data$prestige_score[3],
              data$persistence_score[3],
              data$presence_score[3]
            ),
            theta = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
            name = data$customer_name[3],
            line = list(width = 3),
            marker = list(size = 8),
            hovertemplate = paste(
              '<b>%{fullData.name}</b><br>',
              '%{theta}: %{r:.2f}<br>',
              '<extra></extra>'
            )
          )
        } else {
          .
        }
      } %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),  # Adjust based on your score range
            showline = TRUE,
            showticklabels = TRUE,
            gridcolor = "#E8EDF2"
          ),
          angularaxis = list(
            showline = TRUE,
            gridcolor = "#E8EDF2"
          )
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          yanchor = "bottom",
          y = -0.2,
          xanchor = "center",
          x = 0.5
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF"
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  
  observe({
    all_queries <- dbGetQuery(
      con,
      "SELECT query_id, query_string FROM dim_query ORDER BY query_string"
    )
    
    # Create named vector for dropdown (values are query_string, names are also query_string)
    query_choices <- setNames(all_queries$query_string, all_queries$query_string)
    
    updateSelectInput(
      session,
      "query_select",
      choices = c("Select a query..." = "", query_choices)
    )
  })
  
  # Get top 10 data for selected query
  query_top10_data <- reactive({
    req(input$query_select)
    req(input$query_select != "")
    
    query_selected <- input$query_select
    
    query <- "
    WITH latest_scores AS (
      SELECT 
        dc.customer_name,
        fqh.customer_id,
        fqh.airr_score,
        fqh.presence_score,
        fqh.perception_score,
        fqh.prestige_score,
        fqh.persistence_score,
        fqh.date,
        ROW_NUMBER() OVER (PARTITION BY fqh.customer_id ORDER BY fqh.date DESC) as rn
      FROM fact_query_history fqh
      INNER JOIN dim_cust_query dcq ON fqh.customer_id = dcq.customer_id 
        AND fqh.query_id = dcq.query_id
      INNER JOIN dim_customer dc ON fqh.customer_id = dc.customer_id
      INNER JOIN dim_query dq ON fqh.query_id = dq.query_id
      WHERE dq.query_string = $1
        AND fqh.airr_score IS NOT NULL
    )
    SELECT 
      customer_name,
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
    
    dbGetQuery(con, query, params = list(query_selected))
  })
  
  output$query_top10_table <- renderDT({
    req(query_top10_data())
    
    datatable(
      query_top10_data(),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE,
        ordering = FALSE  # Already ordered by query
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatRound(c('airr_score', 'presence_score', 'perception_score', 'prestige_score', 'persistence_score'), 2) %>%
      formatDate('date', 'toDateString')
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
  
  # Handle query submission
  observeEvent(input$submit_prompt_to_all_btn, {
    # Validate input
    req(input$new_prompt_input)
    
    query_text <- trimws(input$new_prompt_input)
    
    # Check if empty after trimming
    if (query_text == "") {
      showNotification(
        ui = "Please enter a query before submitting.",
        type = "error",
        duration = 3
      )
      return()
    }
    
    # Show processing notification
    notification_id <- showNotification(
      ui = "Adding query for all customers...",
      type = "message",
      duration = NULL,
      closeButton = FALSE
    )
    
    # Use isolate to prevent reactive dependencies
    isolate({
      # Call your function with explicit error handling
      result <- tryCatch({
        query_id <- addPromptForEveryone(query_text)
        list(success = TRUE, query_id = query_id)
      }, error = function(e) {
        list(success = FALSE, error = as.character(e$message))
      })
    })
    
    # Remove processing notification
    removeNotification(id = notification_id)
    
    # Small delay to ensure previous notification is removed
    Sys.sleep(0.1)
    
    # Show result notification with explicit parameters
    if (isTRUE(result$success)) {
      showNotification(
        ui = "Query successfully added for all customers!",
        type = "message",  # Changed from "success" - try "message" instead
        duration = 5,
        closeButton = TRUE
      )
      
      # Clear the input
      
      updateTextInput(session, "new_prompt_input", value = "")
      
      # Refresh the query dropdown with the new query selected
      all_queries <- dbGetQuery(
        con,
        "SELECT query_id, query_string FROM dim_query ORDER BY query_string"
      )
      
      # Create named vector for dropdown
      query_choices <- setNames(all_queries$query_string, all_queries$query_string)
      
      # Update the selectInput with new choices and select the newly added query
      updateSelectInput(
        session,
        "query_select",
        choices = c("Select a query..." = "", query_choices),
        selected = query_text  # Select the newly added query
      )
      
    } else {
      error_text <- if(is.null(result$error)) "Unknown error" else result$error
      showNotification(
        ui = paste("Error adding query:", error_text),
        type = "error",
        duration = 10,
        closeButton = TRUE
      )
    }
  })
  
  
  
}