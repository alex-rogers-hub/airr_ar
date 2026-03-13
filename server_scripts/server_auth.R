# ============================================
# Admin whitelist — no DB column needed
# ============================================

ADMIN_EMAILS <- c("alex@airrscore.com", "steve@airrscore.com")

is_admin <- function(email) {
  tolower(trimws(email)) %in% tolower(ADMIN_EMAILS)
}

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

output$auth_alert <- renderUI({
  if (!is.null(rv$auth_message)) {
    div(
      class = paste0("alert alert-", rv$auth_type),
      rv$auth_message
    )
  }
})

# ============================================
# Helper — load user state from DB
# Sets rv values for a given login_id/email
# Used by both normal login and admin impersonation
# ============================================

.load_user_state <- function(login_id, email) {
  
  ensure_enterprise_subscription(login_id)
  
  ob_check <- dbGetQuery(pool,
                         "SELECT onboarding_complete FROM dim_user WHERE login_id = $1",
                         params = list(login_id))
  
  onboarding_done <- isTRUE(ob_check$onboarding_complete[1])
  
  brand_id_val   <- NULL
  brand_name_val <- NULL
  
  if (onboarding_done) {
    user_brands <- get_user_brands(login_id)
    
    if (nrow(user_brands) > 0) {
      main_brand <- user_brands %>% filter(main_brand_flag == TRUE)
      
      if (nrow(main_brand) > 0) {
        brand_id_val   <- main_brand$brand_id[1]
        brand_name_val <- main_brand$brand_name[1]
      } else {
        brand_id_val   <- user_brands$brand_id[1]
        brand_name_val <- user_brands$brand_name[1]
      }
    } else {
      onboarding_done <- FALSE
      dbExecute(pool,
                "UPDATE dim_user SET onboarding_complete = FALSE WHERE login_id = $1",
                params = list(login_id))
    }
  }
  
  rv$login_id            <- login_id
  rv$email               <- email
  rv$brand_id            <- brand_id_val
  rv$brand_name          <- brand_name_val
  rv$auth_message        <- NULL
  rv$onboarding_complete <- onboarding_done
  rv$logged_in           <- TRUE
}

# ============================================
# Login handler
# ============================================

observeEvent(input$login_btn, {
  email    <- trimws(input$login_email)
  password <- input$login_password
  
  if (email == "" || password == "") {
    rv$auth_message <- "Please enter both email and password"
    rv$auth_type    <- "danger"
    return()
  }
  
  # Demo account shortcut
  if (email == "demo@airr.com" && password == "Demo") {
    rv$login_id            <- 0
    rv$email               <- "demo@airr.com"
    rv$brand_id            <- 0
    rv$brand_name          <- "Demo"
    rv$auth_message        <- NULL
    rv$onboarding_complete <- TRUE
    rv$logged_in           <- TRUE
    rv$is_admin            <- FALSE
    rv$admin_email         <- NULL
    rv$admin_login_id      <- NULL
    showNotification("Login successful!", type = "message", duration = 3)
    return()
  }
  
  user <- verify_user(email, password)
  
  if (!is.null(user)) {
    
    .load_user_state(user$login_id, user$email)
    
    # Set admin flag — store real admin identity separately
    if (is_admin(user$email)) {
      rv$is_admin       <- TRUE
      rv$admin_email    <- user$email
      rv$admin_login_id <- user$login_id
      showNotification("Logged in as admin", type = "message", duration = 3)
    } else {
      rv$is_admin       <- FALSE
      rv$admin_email    <- NULL
      rv$admin_login_id <- NULL
      showNotification("Login successful!", type = "message", duration = 3)
    }
    
  } else {
    rv$auth_message <- "Invalid email or password"
    rv$auth_type    <- "danger"
  }
})

# ============================================
# Register handler
# ============================================

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
  ensure_enterprise_subscription(login_id)
  
  if (!is.null(login_id)) {
    rv$login_id            <- login_id
    rv$email               <- email
    rv$brand_id            <- NULL
    rv$brand_name          <- NULL
    rv$auth_message        <- NULL
    rv$onboarding_complete <- FALSE
    rv$logged_in           <- TRUE
    rv$is_admin            <- FALSE
    rv$admin_email         <- NULL
    rv$admin_login_id      <- NULL
  } else {
    rv$auth_message <- "Email already exists"
    rv$auth_type    <- "danger"
  }
})

# ============================================
# Logout handler
# ============================================

observeEvent(input$logout, {
  rv$logged_in           <- FALSE
  rv$login_id            <- NULL
  rv$email               <- NULL
  rv$brand_id            <- NULL
  rv$brand_name          <- NULL
  rv$onboarding_complete <- FALSE
  rv$auth_message        <- NULL
  rv$is_admin            <- FALSE
  rv$admin_email         <- NULL
  rv$admin_login_id      <- NULL
  
  showNotification("Logged out successfully", type = "message", duration = 3)
})

# ============================================
# Admin — user switcher
# ============================================

# Fetch all users for the switcher dropdown
admin_user_list <- reactive({
  req(rv$is_admin)
  
  dbGetQuery(pool, "
    SELECT 
      u.login_id,
      u.email,
      u.onboarding_complete,
      b.brand_name,
      ds.subscription_name
    FROM dim_user u
    LEFT JOIN fact_user_brands_tracked ubt 
      ON ubt.login_id = u.login_id 
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
    LEFT JOIN dim_brand b ON b.brand_id = ubt.brand_id
    LEFT JOIN fact_user_sub_level fus 
      ON fus.login_id = u.login_id
      AND fus.date_valid_from <= CURRENT_DATE
      AND (fus.date_valid_to IS NULL OR fus.date_valid_to > CURRENT_DATE)
    LEFT JOIN dim_subscription ds 
      ON ds.subscription_level_id = fus.subscription_level_id
    ORDER BY u.login_id DESC
  ")
})

# Render the admin switcher bar — only visible to admins
output$admin_switcher_ui <- renderUI({
  req(rv$is_admin)
  
  users         <- admin_user_list()
  is_impersonating <- !is.null(rv$admin_login_id) && rv$login_id != rv$admin_login_id
  
  choices <- setNames(
    users$login_id,
    paste0(
      users$email,
      ifelse(!is.na(users$brand_name),
             paste0(" — ", users$brand_name), ""),
      ifelse(!is.na(users$subscription_name),
             paste0(" (", users$subscription_name, ")"), "")
    )
  )
  
  tagList(
    # JS to add class to body so CSS can target everything below
    tags$script(HTML("
      document.body.classList.add('admin-bar-visible');
    ")),
    
    div(
      id = "admin_switcher_bar",
      style = paste0(
        "position: fixed; top: 0; left: 0; right: 0; z-index: 9999; ",
        "background: linear-gradient(135deg, #2C0A5E 0%, #4A148C 100%); ",
        "color: white; padding: 6px 16px; ",
        "display: flex; align-items: center; gap: 12px; ",
        "font-size: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.3); ",
        "height: 38px;"
      ),
      
      div(
        style = "display: flex; align-items: center; gap: 6px; flex-shrink: 0;",
        icon("shield-halved", style = "color: #CE93D8;"),
        tags$span(
          style = "font-weight: 700; color: #CE93D8; text-transform: uppercase; 
                   letter-spacing: 1px; font-size: 11px;",
          "Admin"
        ),
        tags$span(style = "color: #9C4DCC; font-size: 11px;", rv$admin_email)
      ),
      
      div(style = "width: 1px; height: 20px; background: rgba(255,255,255,0.2); flex-shrink: 0;"),
      
      tags$span(
        style = "color: #E1BEE7; flex-shrink: 0;",
        if (is_impersonating) "Viewing as:" else "View as user:"
      ),
      
      div(
        style = "flex: 1; max-width: 400px;",
        selectInput(
          "admin_user_select",
          NULL,
          choices  = c("— select a user —" = "", choices),
          selected = if (is_impersonating) rv$login_id else "",
          width    = "100%"
        )
      ),
      
      actionButton(
        "admin_impersonate_btn",
        "View",
        icon  = icon("eye"),
        style = "background: #7B1FA2; color: white; border: none;
                 border-radius: 6px; padding: 4px 14px; font-size: 12px;
                 font-weight: 600; flex-shrink: 0;"
      ),
      
      if (is_impersonating) {
        actionButton(
          "admin_return_btn",
          "Return to my account",
          icon  = icon("arrow-left"),
          style = "background: rgba(255,255,255,0.15); color: white; border: none;
                   border-radius: 6px; padding: 4px 14px; font-size: 12px;
                   font-weight: 600; flex-shrink: 0;"
        )
      },
      
      if (is_impersonating) {
        div(
          style = "margin-left: auto; display: flex; align-items: center; gap: 6px;
                   background: rgba(255,255,255,0.1); border-radius: 6px; 
                   padding: 4px 10px; flex-shrink: 0;",
          icon("eye", style = "color: #CE93D8; font-size: 11px;"),
          tags$span(
            style = "color: #E1BEE7; font-size: 11px;",
            paste0("Viewing: ", rv$email,
                   if (!is.null(rv$brand_name)) paste0(" (", rv$brand_name, ")") else "")
          )
        )
      }
    )
  )
})

# Impersonate selected user
observeEvent(input$admin_impersonate_btn, {
  req(rv$is_admin)
  
  selected_id <- as.integer(input$admin_user_select)
  
  if (is.na(selected_id) || selected_id == 0) {
    showNotification("Please select a user first.", type = "warning", duration = 3)
    return()
  }
  
  # Look up their email
  user_row <- dbGetQuery(pool,
                         "SELECT login_id, email FROM dim_user WHERE login_id = $1",
                         params = list(selected_id))
  
  if (nrow(user_row) == 0) {
    showNotification("User not found.", type = "error", duration = 3)
    return()
  }
  
  # Load that user's state into rv (admin flags stay intact)
  .load_user_state(user_row$login_id[1], user_row$email[1])
  
  showNotification(
    paste0("Now viewing as: ", user_row$email[1]),
    type = "message", duration = 4
  )
})

# Return to admin's own account
observeEvent(input$admin_return_btn, {
  req(rv$is_admin, rv$admin_login_id)
  
  admin_row <- dbGetQuery(pool,
                          "SELECT login_id, email FROM dim_user WHERE login_id = $1",
                          params = list(rv$admin_login_id))
  
  if (nrow(admin_row) == 0) return()
  
  .load_user_state(admin_row$login_id[1], admin_row$email[1])
  
  showNotification("Returned to your own account.", type = "message", duration = 3)
})

# ============================================
# Header display
# ============================================

output$user_info <- renderUI({
  if (rv$logged_in) {
    
    is_impersonating <- isTRUE(rv$is_admin) && 
      !is.null(rv$admin_login_id) && 
      rv$login_id != rv$admin_login_id
    
    tags$span(
      style = "color: white; padding-right: 10px; font-weight: 500;",
      if (is_impersonating) {
        tagList(
          icon("eye", style = "color: #CE93D8;"),
          tags$span(style = "color: #CE93D8; margin-left: 4px;",
                    if (!is.null(rv$brand_name)) rv$brand_name else rv$email)
        )
      } else {
        tagList(
          icon("user-circle"),
          " ",
          if (!is.null(rv$brand_name)) rv$brand_name else rv$email
        )
      }
    )
  }
})

# ============================================
# UI panel visibility controller
# ============================================

observe({
  logged_in       <- rv$logged_in
  onboarding_done <- rv$onboarding_complete
  
  if (!logged_in) {
    shinyjs::show("login_panel")
    shinyjs::hide("onboarding_panel")
    shinyjs::hide("main_app_panel")
  } else if (!onboarding_done) {
    shinyjs::hide("login_panel")
    shinyjs::show("onboarding_panel")
    shinyjs::hide("main_app_panel")
  } else {
    shinyjs::hide("login_panel")
    shinyjs::hide("onboarding_panel")
    shinyjs::show("main_app_panel")
  }
})


