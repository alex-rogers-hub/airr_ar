# ============================================
# Authentication
# ============================================

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

output$logged_in <- reactive({
  rv$logged_in
})
outputOptions(output, "logged_in", suspendWhenHidden = FALSE)

output$auth_alert <- renderUI({
  if (!is.null(rv$auth_message)) {
    div(
      class = paste0("alert alert-", rv$auth_type),
      rv$auth_message
    )
  }
})

# Login handler
observeEvent(input$login_btn, {
  email <- trimws(input$login_email)
  password <- input$login_password
  
  if (email == "" || password == "") {
    rv$auth_message <- "Please enter both email and password"
    rv$auth_type <- "danger"
    return()
  }
  
  if (email == "demo@airr.com" && password == "Demo") {
    rv$logged_in <- TRUE
    rv$login_id <- 0
    rv$email <- "demo@airr.com"
    rv$brand_id <- 0
    rv$brand_name <- "Demo"
    rv$auth_message <- NULL
    
    showNotification("Login successful!", type = "message", duration = 3)
  } else {
    user <- verify_user(email, password)
    
    if (!is.null(user)) {
      rv$logged_in <- TRUE
      rv$login_id <- user$login_id
      rv$email <- user$email
      rv$auth_message <- NULL
      
      # Get the user's main brand
      user_brands <- get_user_brands(user$login_id)
      main_brand <- user_brands %>% filter(main_brand_flag == TRUE)
      
      if (nrow(main_brand) > 0) {
        rv$brand_id <- main_brand$brand_id[1]
        rv$brand_name <- main_brand$brand_name[1]
      } else if (nrow(user_brands) > 0) {
        rv$brand_id <- user_brands$brand_id[1]
        rv$brand_name <- user_brands$brand_name[1]
      }
      
      showNotification("Login successful!", type = "message", duration = 3)
    } else {
      rv$auth_message <- "Invalid email or password"
      rv$auth_type <- "danger"
    }
  }
})

# Register handler
observeEvent(input$register_btn, {
  brand_name <- trimws(input$register_brand_name)
  email <- trimws(input$register_email)
  password <- input$register_password
  password_confirm <- input$register_password_confirm
  
  if (brand_name == "" || email == "" || password == "" || password_confirm == "") {
    rv$auth_message <- "Please fill in all fields"
    rv$auth_type <- "danger"
    return()
  }
  
  if (password != password_confirm) {
    rv$auth_message <- "Passwords do not match"
    rv$auth_type <- "danger"
    return()
  }
  
  if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$", email)) {
    rv$auth_message <- "Please enter a valid email address"
    rv$auth_type <- "danger"
    return()
  }
  
  # Create user account
  login_id <- create_user(email, password)
  
  if (!is.null(login_id)) {
    # Create/link brand — checks if it already exists and has scores
    brand_result <- add_brand_for_user_pending(login_id, brand_name, main_brand = TRUE)
    
    if (!is.null(brand_result)) {
      rv$logged_in <- TRUE
      rv$login_id <- login_id
      rv$email <- email
      rv$brand_id <- brand_result$brand_id
      rv$brand_name <- brand_name
      rv$auth_message <- NULL
      
      if (brand_result$needs_scoring) {
        # Brand is new — calculate scores
        showNotification(
          "Account created! Calculating your brand scores — this may take a minute...",
          type = "message", duration = 5
        )
        
        # Run scoring in background
        brand_name_copy <- brand_name
        
        future_promise({
          bg_con <- dbConnect(
            RPostgres::Postgres(),
            dbname = Sys.getenv("DB_NAME"),
            host = Sys.getenv("DB_HOST"),
            port = Sys.getenv("DB_PORT"),
            user = Sys.getenv("DB_USER"),
            password = Sys.getenv("DB_PASSWORD")
          )
          on.exit(dbDisconnect(bg_con))
          assign("con", bg_con, envir = globalenv())
          
          tryCatch({
            user_create_airr(brand_name_copy)
            list(success = TRUE, brand = brand_name_copy)
          }, error = function(e) {
            list(success = FALSE, brand = brand_name_copy, error = e$message)
          })
        }) %...>% (function(result) {
          if (result$success) {
            showNotification(
              paste0("✓ Scores ready for ", result$brand, "!"),
              type = "message", duration = 5
            )
          } else {
            showNotification(
              paste0("⚠ Score calculation failed for ", result$brand, ": ", result$error),
              type = "warning", duration = 10
            )
          }
          rv$brands_refresh <- rv$brands_refresh + 1
        }) %...!% (function(err) {
          showNotification(
            paste0("⚠ Background error: ", err$message),
            type = "error", duration = 10
          )
          rv$brands_refresh <- rv$brands_refresh + 1
        })
        
      } else {
        # Brand already has scores — instant
        showNotification(
          "Account created! Your brand already has scores on file.",
          type = "message", duration = 3
        )
      }
    } else {
      rv$auth_message <- "Error setting up brand. Please try again."
      rv$auth_type <- "danger"
    }
  } else {
    rv$auth_message <- "Email already exists"
    rv$auth_type <- "danger"
  }
})

# Logout handler
observeEvent(input$logout, {
  rv$logged_in <- FALSE
  rv$login_id <- NULL
  rv$email <- NULL
  rv$brand_id <- NULL
  rv$brand_name <- NULL
  rv$auth_message <- NULL
  
  showNotification("Logged out successfully", type = "message", duration = 3)
})

# User info display
output$user_info <- renderUI({
  if (rv$logged_in) {
    tags$span(
      style = "color: white; padding-right: 10px; font-weight: 500;",
      icon("user-circle"),
      " ",
      rv$brand_name
    )
  }
})