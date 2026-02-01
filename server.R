source("global.R")
source("custom_css_stuff.R")

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    logged_in = FALSE,
    customer_id = NULL,
    customer_name = NULL,
    email = NULL,
    auth_message = NULL,
    auth_type = NULL
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
  
  user_timeseries <- reactive({
    req(rv$logged_in, rv$customer_id)
    get_user_timeseries(rv$customer_id)
  })
  
  # Value boxes
  output$metric_box_1 <- renderValueBox({
    req(user_metrics())
    
    # Calculate total score or some aggregate metric
    total <- sum(user_metrics()$metric_value, na.rm = TRUE)
    
    valueBox(
      value = format(round(total, 1), big.mark = ","),
      subtitle = "Total Score",
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
  
  output$metric_box_3 <- renderValueBox({
    req(user_timeseries())
    
    # Count data points
    count <- nrow(user_timeseries())
    
    valueBox(
      value = count,
      subtitle = "Data Points",
      icon = icon("database"),
      color = "yellow"
    )
  })
  
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
    req(user_timeseries())
    
    data <- user_timeseries() %>%
      arrange(recorded_at)
    
    # Create multiple traces for different metrics
    plot_ly() %>%
      add_trace(
        data = data,
        x = ~recorded_at,
        y = ~metric_value,
        color = ~metric_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 3),
        marker = list(size = 8),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Value: %{y:.2f}<br>',
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
          y = -0.3,
          xanchor = "center",
          x = 0.5
        ),
        margin = list(l = 50, r = 50, t = 30, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Spider/Radar chart
  output$spider_chart <- renderPlotly({
    req(user_metrics())
    
    # Get latest metrics by category
    latest_metrics <- user_metrics() %>%
      group_by(category) %>%
      summarise(
        value = mean(metric_value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(category)
    
    # Normalize to 0-100 scale
    latest_metrics$normalized <- scales::rescale(latest_metrics$value, to = c(0, 100))
    
    plot_ly(
      type = 'scatterpolar',
      r = latest_metrics$normalized,
      theta = latest_metrics$category,
      fill = 'toself',
      fillcolor = 'rgba(102, 126, 234, 0.5)',
      line = list(
        color = app_colors$secondary,
        width = 3
      ),
      marker = list(
        size = 8,
        color = app_colors$secondary
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
  
  # Update metric selector for performance tab
  observe({
    req(user_timeseries())
    
    metrics <- unique(user_timeseries()$metric_name)
    
    updateSelectInput(
      session,
      "metric_selector",
      choices = metrics,
      selected = metrics[1:min(3, length(metrics))]
    )
  })
  
  # Detailed time series
  output$detailed_timeseries <- renderPlotly({
    req(user_timeseries(), input$metric_selector)
    
    data <- user_timeseries() %>%
      filter(metric_name %in% input$metric_selector) %>%
      arrange(recorded_at)
    
    plot_ly() %>%
      add_trace(
        data = data,
        x = ~recorded_at,
        y = ~metric_value,
        color = ~metric_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(width = 3),
        marker = list(size = 10),
        hovertemplate = paste(
          '<b>%{fullData.name}</b><br>',
          'Date: %{x|%Y-%m-%d}<br>',
          'Value: %{y:.2f}<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        title = list(
          text = "Detailed Metric Trends",
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
          y = -0.3,
          xanchor = "center",
          x = 0.5
        ),
        margin = list(l = 60, r = 50, t = 60, b = 100)
      ) %>%
      config(displayModeBar = TRUE)
  })
  
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