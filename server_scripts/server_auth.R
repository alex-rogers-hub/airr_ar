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
  email    <- trimws(input$login_email)
  password <- input$login_password
  
  if (email == "" || password == "") {
    rv$auth_message <- "Please enter both email and password"
    rv$auth_type    <- "danger"
    return()
  }
  
  if (email == "demo@airr.com" && password == "Demo") {
    rv$login_id             <- 0        # SET login_id FIRST
    rv$email                <- "demo@airr.com"
    rv$brand_id             <- 0
    rv$brand_name           <- "Demo"
    rv$auth_message         <- NULL
    rv$onboarding_complete  <- TRUE
    rv$logged_in            <- TRUE     # SET logged_in LAST
    showNotification("Login successful!", type = "message", duration = 3)
    return()
  }
  
  user <- verify_user(email, password)
  
  if (!is.null(user)) {
    
    # Check onboarding status first, before setting logged_in
    ob_check <- dbGetQuery(pool,
                           "SELECT onboarding_complete FROM dim_user WHERE login_id = $1",
                           params = list(user$login_id))
    
    onboarding_done <- isTRUE(ob_check$onboarding_complete[1])
    
    # Load brand if onboarding done
    brand_id_val   <- NULL
    brand_name_val <- NULL
    
    if (onboarding_done) {
      user_brands <- get_user_brands(user$login_id)
      main_brand  <- user_brands %>% filter(main_brand_flag == TRUE)
      
      if (nrow(main_brand) > 0) {
        brand_id_val   <- main_brand$brand_id[1]
        brand_name_val <- main_brand$brand_name[1]
      } else if (nrow(user_brands) > 0) {
        brand_id_val   <- user_brands$brand_id[1]
        brand_name_val <- user_brands$brand_name[1]
      }
    }
    
    # Now set ALL reactive values together, login_id before logged_in
    rv$login_id            <- user$login_id
    rv$email               <- user$email
    rv$auth_message        <- NULL
    rv$onboarding_complete <- onboarding_done
    rv$brand_id            <- brand_id_val
    rv$brand_name          <- brand_name_val
    rv$logged_in           <- TRUE      # SET logged_in LAST so reactives have all values
    
    showNotification("Login successful!", type = "message", duration = 3)
    
  } else {
    rv$auth_message <- "Invalid email or password"
    rv$auth_type    <- "danger"
  }
})

# Register handler — remove brand_name field, onboarding handles that now
observeEvent(input$register_btn, {
  email            <- trimws(input$register_email)
  password         <- input$register_password
  password_confirm <- input$register_password_confirm
  
  if (email == "" || password == "" || password_confirm == "") {
    rv$auth_message <- "Please fill in all fields"
    rv$auth_type    <- "danger"
    return()
  }
  
  if (password != password_confirm) {
    rv$auth_message <- "Passwords do not match"
    rv$auth_type    <- "danger"
    return()
  }
  
  if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$", email)) {
    rv$auth_message <- "Please enter a valid email address"
    rv$auth_type    <- "danger"
    return()
  }
  
  login_id <- create_user(email, password)
  
  if (!is.null(login_id)) {
    # SET login_id before logged_in
    rv$login_id            <- login_id
    rv$email               <- email
    rv$auth_message        <- NULL
    rv$onboarding_complete <- FALSE
    rv$logged_in           <- TRUE     # LAST
  } else {
    rv$auth_message <- "Email already exists"
    rv$auth_type    <- "danger"
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

output$onboarding_complete <- reactive({ rv$onboarding_complete })
outputOptions(output, "onboarding_complete", suspendWhenHidden = FALSE)
