# ============================================
# Admin whitelist — no DB column needed
# ============================================

ADMIN_EMAILS <- c("alex@airrscore.com", "steve@airrscore.com")

is_admin <- function(email) {
  tolower(trimws(email)) %in% tolower(ADMIN_EMAILS)
}

ONBOARDING_DEMO_EMAIL <- "onboarding@airrscore.com"

is_onboarding_account <- function(email) {
  tolower(trimws(email)) == tolower(ONBOARDING_DEMO_EMAIL)
}

# ============================================
# Demo user helpers
# ============================================

is_demo_user <- function(email) {
  result <- dbGetQuery(pool,
                       "SELECT is_demo FROM dim_user WHERE email = $1",
                       params = list(email))
  nrow(result) > 0 && isTRUE(result$is_demo[1])
}

get_demo_targets <- function(demo_login_id) {
  dbGetQuery(pool, "
    SELECT da.target_login_id, u.email, b.brand_name
    FROM dim_demo_accounts da
    JOIN dim_user u ON u.login_id = da.target_login_id
    LEFT JOIN fact_user_brands_tracked ubt
      ON ubt.login_id = da.target_login_id
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LEFT JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE da.demo_login_id = $1
    ORDER BY da.display_order",
             params = list(demo_login_id))
}

# ============================================
# Authentication UI outputs
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
# Used by normal login, admin impersonation and demo switching
# ============================================

.load_user_state <- function(login_id, email) {
  
  ensure_free_subscription(login_id)
  
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
  
  user <- verify_user(email, password)
  
  if (is.null(user)) {
    rv$auth_message <- "Invalid email or password"
    rv$auth_type    <- "danger"
    return()
  }
  
  # ── Demo user ────────────────────────────────────────────────────────
  if (is_demo_user(user$email)) {
    
    targets <- get_demo_targets(user$login_id)
    
    if (nrow(targets) == 0) {
      rv$auth_message <- "Demo account not configured. Please contact support."
      rv$auth_type    <- "danger"
      return()
    }
    
    # Load the first target account
    first_target <- targets[1, ]
    .load_user_state(first_target$target_login_id, first_target$email)
    
    # Store demo identity
    rv$is_demo        <- TRUE
    rv$demo_login_id  <- user$login_id
    rv$demo_targets   <- targets
    rv$is_admin       <- FALSE
    rv$admin_email    <- NULL
    rv$admin_login_id <- NULL
    
    showNotification("Welcome to the AiRR demo!",
                     type = "message", duration = 4)
    return()
  }
  
  # ── Admin user ───────────────────────────────────────────────────────
  .load_user_state(user$login_id, user$email)
  
  rv$is_demo       <- FALSE
  rv$demo_login_id <- NULL
  rv$demo_targets  <- NULL
  
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
  ensure_free_subscription(login_id)
  
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
    rv$is_demo             <- FALSE
    rv$demo_login_id       <- NULL
    rv$demo_targets        <- NULL
  } else {
    rv$auth_message <- "Email already exists"
    rv$auth_type    <- "danger"
  }
})

# ============================================
# Logout handler
# ============================================

observeEvent(input$logout, {
  
  if (!is.null(rv$email) && is_onboarding_account(rv$email)) {
    message("Onboarding account logging out — resetting...")
    reset_onboarding_account(rv$login_id)
  }
  
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
  rv$is_demo             <- FALSE
  rv$demo_login_id       <- NULL
  rv$demo_targets        <- NULL
  
  showNotification("Logged out successfully", type = "message", duration = 3)
})

# ============================================
# Demo — switcher bar
# ============================================

output$demo_switcher_ui <- renderUI({
  req(rv$is_demo)
  
  targets <- rv$demo_targets
  if (is.null(targets) || nrow(targets) == 0) return(NULL)
  
  choices <- setNames(
    targets$target_login_id,
    ifelse(!is.na(targets$brand_name) & nzchar(targets$brand_name %||% ""),
           targets$brand_name,
           targets$email)
  )
  
  tagList(
    tags$script(HTML("document.body.classList.add('demo-bar-visible');")),
    
    div(
      id = "demo_switcher_bar",
      style = "position: fixed; top: 0; left: 0; right: 0; z-index: 9999;
               background: linear-gradient(135deg, #1A1A1A 0%, #2D2D2D 100%);
               border-bottom: 2px solid #D4A843;
               color: white; padding: 6px 16px;
               display: flex; align-items: center; gap: 12px;
               font-size: 12px; height: 38px;",
      
      # Demo badge
      div(
        style = "display: flex; align-items: center; gap: 6px; flex-shrink: 0;",
        icon("eye", style = "color: #D4A843;"),
        tags$span(
          style = "font-weight: 700; color: #D4A843; text-transform: uppercase;
                   letter-spacing: 1px; font-size: 11px;",
          "Demo Mode"
        ),
        tags$span(
          style = "color: #9E9E9E; font-size: 11px;",
          "\u2014 read only view"
        )
      ),
      
      div(style = "width: 1px; height: 20px;
                   background: rgba(255,255,255,0.2); flex-shrink: 0;"),
      
      tags$span(style = "color: #a0aec0; flex-shrink: 0;", "Viewing:"),
      
      # Account switcher
      div(
        style = "flex: 1; max-width: 300px;",
        selectInput(
          "demo_account_select",
          NULL,
          choices  = choices,
          selected = rv$login_id,
          width    = "100%"
        )
      ),
      
      actionButton(
        "demo_switch_btn", "Switch",
        icon  = icon("arrows-rotate"),
        style = "background: rgba(212,168,67,0.2); color: #D4A843; border: none;
                 border-radius: 6px; padding: 4px 14px; font-size: 12px;
                 font-weight: 600; flex-shrink: 0;"
      ),
      
      # Get started CTA
      div(
        style = "margin-left: auto; flex-shrink: 0;",
        tags$a(
          href   = "https://airrscore.com/pricing",
          target = "_blank",
          style  = "background: #D4A843; color: #1A1A1A; border-radius: 6px;
                     padding: 5px 14px; font-size: 12px; font-weight: 700;
                     text-decoration: none; display: inline-flex;
                     align-items: center; gap: 6px;",
          icon("rocket", style = "font-size: 11px;"),
          "Get Started"
        )
      )
    )
  )
})

# Switch demo account
observeEvent(input$demo_switch_btn, {
  req(rv$is_demo, rv$demo_targets)
  
  selected_id <- as.integer(input$demo_account_select)
  if (is.na(selected_id)) return()
  
  targets    <- rv$demo_targets
  target_row <- targets[targets$target_login_id == selected_id, ]
  if (nrow(target_row) == 0) return()
  
  # Store demo state before .load_user_state overwrites it
  stored_demo_login_id <- isolate(rv$demo_login_id)
  stored_demo_targets  <- targets
  
  .load_user_state(selected_id, target_row$email[1])
  
  # Restore demo flags
  rv$is_demo       <- TRUE
  rv$demo_login_id <- stored_demo_login_id
  rv$demo_targets  <- stored_demo_targets
  rv$is_admin      <- FALSE
  
  showNotification(
    paste0("Now viewing: ",
           if (!is.na(target_row$brand_name[1]) &&
               nzchar(target_row$brand_name[1] %||% ""))
             target_row$brand_name[1] else target_row$email[1]),
    type = "message", duration = 3
  )
})

# Block account tab for demo users
observe({
  req(rv$is_demo)
  if (!is.null(input$sidebar) && input$sidebar == "account") {
    updateTabItems(session, "sidebar", "brand_overview")
    showNotification(
      "Account settings are not available in demo mode.",
      type = "warning", duration = 3
    )
  }
})

# ============================================
# Admin — user switcher
# ============================================

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
    -- Must have at least one active brand being tracked
    INNER JOIN fact_user_brands_tracked ubt
      ON ubt.login_id = u.login_id
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
    LEFT JOIN dim_brand b 
      ON b.brand_id = ubt.brand_id
    LEFT JOIN fact_user_sub_level fus 
      ON fus.login_id = u.login_id
      AND fus.date_valid_from <= CURRENT_DATE
      AND (fus.date_valid_to IS NULL OR fus.date_valid_to > CURRENT_DATE)
    LEFT JOIN dim_subscription ds 
      ON ds.subscription_level_id = fus.subscription_level_id
    -- Exclude demo accounts and admin accounts
    WHERE u.is_demo = FALSE
      AND u.email NOT IN (SELECT unnest($1::text[]))
      AND u.onboarding_complete = TRUE
    ORDER BY u.login_id DESC
  ", params = list(paste0('{', 
                          paste(c(ADMIN_EMAILS, ONBOARDING_DEMO_EMAIL), collapse = ','), 
                          '}')))
})

output$admin_switcher_ui <- renderUI({
  req(rv$is_admin)
  
  users            <- admin_user_list()
  is_impersonating <- !is.null(rv$admin_login_id) &&
    rv$login_id != rv$admin_login_id
  
  choices <- setNames(
    users$login_id,
    paste0(
      users$email,
      ifelse(!is.na(users$brand_name),
             paste0(" \u2014 ", users$brand_name), ""),
      ifelse(!is.na(users$subscription_name),
             paste0(" (", users$subscription_name, ")"), "")
    )
  )
  
  tagList(
    tags$script(HTML("document.body.classList.add('admin-bar-visible');")),
    
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
      
      div(style = "width: 1px; height: 20px;
                   background: rgba(255,255,255,0.2); flex-shrink: 0;"),
      
      tags$span(
        style = "color: #E1BEE7; flex-shrink: 0;",
        if (is_impersonating) "Viewing as:" else "View as user:"
      ),
      
      div(
        style = "flex: 1; max-width: 400px;",
        selectInput(
          "admin_user_select",
          NULL,
          choices  = c("\u2014 select a user \u2014" = "", choices),
          selected = if (is_impersonating) rv$login_id else "",
          width    = "100%"
        )
      ),
      
      actionButton(
        "admin_impersonate_btn", "View",
        icon  = icon("eye"),
        style = "background: #7B1FA2; color: white; border: none;
                 border-radius: 6px; padding: 4px 14px; font-size: 12px;
                 font-weight: 600; flex-shrink: 0;"
      ),
      
      if (is_impersonating) {
        actionButton(
          "admin_return_btn", "Return to my account",
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
                   if (!is.null(rv$brand_name))
                     paste0(" (", rv$brand_name, ")") else "")
          )
        )
      }
    )
  )
})

observeEvent(input$admin_impersonate_btn, {
  req(rv$is_admin)
  
  selected_id <- as.integer(input$admin_user_select)
  
  if (is.na(selected_id) || selected_id == 0) {
    showNotification("Please select a user first.",
                     type = "warning", duration = 3)
    return()
  }
  
  user_row <- dbGetQuery(pool,
                         "SELECT login_id, email FROM dim_user WHERE login_id = $1",
                         params = list(selected_id))
  
  if (nrow(user_row) == 0) {
    showNotification("User not found.", type = "error", duration = 3)
    return()
  }
  
  .load_user_state(user_row$login_id[1], user_row$email[1])
  
  showNotification(
    paste0("Now viewing as: ", user_row$email[1]),
    type = "message", duration = 4
  )
})

observeEvent(input$admin_return_btn, {
  req(rv$is_admin, rv$admin_login_id)
  
  admin_row <- dbGetQuery(pool,
                          "SELECT login_id, email FROM dim_user WHERE login_id = $1",
                          params = list(rv$admin_login_id))
  
  if (nrow(admin_row) == 0) return()
  
  .load_user_state(admin_row$login_id[1], admin_row$email[1])
  
  showNotification("Returned to your own account.",
                   type = "message", duration = 3)
})

# ============================================
# Header display
# ============================================

output$user_info <- renderUI({
  if (rv$logged_in) {
    
    is_impersonating <- isTRUE(rv$is_admin) &&
      !is.null(rv$admin_login_id) &&
      rv$login_id != rv$admin_login_id
    
    is_demo <- isTRUE(rv$is_demo)
    
    tags$span(
      style = "color: white; padding-right: 10px; font-weight: 500;",
      if (is_demo) {
        tagList(
          icon("eye", style = "color: #D4A843;"),
          tags$span(style = "color: #D4A843; margin-left: 4px;",
                    if (!is.null(rv$brand_name)) rv$brand_name else rv$email)
        )
      } else if (is_impersonating) {
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

# ============================================
# Stripe payment return
# ============================================

observeEvent(input$payment_status, {
  if (input$payment_status == "success") {
    showNotification(
      "\u2713 Payment successful! Your account has been upgraded.",
      type = "message", duration = 6
    )
    rv$brands_refresh <- rv$brands_refresh + 1
  }
})


# ============================================
# Forgot password handler
# ============================================

output$forgot_alert <- renderUI({ NULL })

observeEvent(input$forgot_btn, {
  email <- trimws(input$forgot_email %||% "")
  
  if (!nzchar(email)) {
    output$forgot_alert <- renderUI({
      div(class = "alert alert-danger", "Please enter your email address.")
    })
    return()
  }
  
  result <- tryCatch(
    generate_reset_token(email),
    error = function(e) {
      message("generate_reset_token error: ", e$message)
      NULL
    }
  )
  
  # Only send email if token was successfully generated
  if (!is.null(result) && isTRUE(result$success) && !is.null(result$token)) {
    app_url <- "https://airr.airrscore.com/AiRR/"
    tryCatch(
      send_reset_email(email, result$token, app_url),
      error = function(e) message("send_reset_email error: ", e$message)
    )
  }
  
  # Always show same message — don't reveal whether email exists
  output$forgot_alert <- renderUI({
    div(
      class = "alert alert-success",
      "If that email is registered, you'll receive a reset link shortly."
    )
  })
  
  updateTextInput(session, "forgot_email", value = "")
})

# ============================================
# Reset password handler
# ============================================

output$reset_alert <- renderUI({ NULL })

# Store token when it arrives from URL
reset_token_val <- reactiveVal(NULL)

observeEvent(input$url_reset_token, {
  token <- input$url_reset_token
  if (!is.null(token) && nzchar(token)) {
    # Validate immediately
    token_row <- validate_reset_token(token)
    if (is.null(token_row)) {
      output$reset_alert <- renderUI({
        div(class = "alert alert-danger",
            "This reset link is invalid or has expired. Please request a new one.")
      })
      shinyjs::runjs(
        "Shiny.setInputValue('show_reset_form', false, {priority:'event'});"
      )
    } else {
      reset_token_val(token)
    }
  }
})

observeEvent(input$reset_btn, {
  req(input$url_reset_token)
  
  password         <- input$reset_password
  password_confirm <- input$reset_password_confirm
  token            <- input$url_reset_token
  
  # Validation
  if (!nzchar(password %||% "")) {
    output$reset_alert <- renderUI({
      div(class = "alert alert-danger", "Please enter a new password.")
    })
    return()
  }
  
  if (nchar(password) < 8) {
    output$reset_alert <- renderUI({
      div(class = "alert alert-danger",
          "Password must be at least 8 characters.")
    })
    return()
  }
  
  if (!grepl("[A-Z]", password)) {
    output$reset_alert <- renderUI({
      div(class = "alert alert-danger",
          "Password must contain at least one capital letter.")
    })
    return()
  }
  
  if (!grepl("[0-9]", password)) {
    output$reset_alert <- renderUI({
      div(class = "alert alert-danger",
          "Password must contain at least one number.")
    })
    return()
  }
  
  if (!grepl("[^A-Za-z0-9]", password)) {
    output$reset_alert <- renderUI({
      div(class = "alert alert-danger",
          "Password must contain at least one symbol.")
    })
    return()
  }
  
  if (password != password_confirm) {
    output$reset_alert <- renderUI({
      div(class = "alert alert-danger", "Passwords do not match.")
    })
    return()
  }
  
  # Validate token and update password
  result <- tryCatch({
    
    token_row <- dbGetQuery(pool,
                            "SELECT login_id, expires_at, used
       FROM dim_password_reset_tokens
       WHERE token = $1",
                            params = list(token))
    
    if (nrow(token_row) == 0) {
      return(list(success = FALSE,
                  message = "This reset link is invalid or has already been used."))
    }
    
    if (isTRUE(token_row$used[1])) {
      return(list(success = FALSE,
                  message = "This reset link has already been used."))
    }
    
    if (as.POSIXct(token_row$expires_at[1]) < Sys.time()) {
      return(list(success = FALSE,
                  message = "This reset link has expired. Please request a new one."))
    }
    
    login_id      <- token_row$login_id[1]
    password_hash <- digest::digest(password, algo = "sha256")
    
    # Update password
    dbExecute(pool,
              "UPDATE dim_user SET password_hash = $1 WHERE login_id = $2",
              params = list(password_hash, login_id))
    
    # Mark token as used
    dbExecute(pool,
              "UPDATE dim_password_reset_tokens SET used = TRUE WHERE token = $1",
              params = list(token))
    
    list(success = TRUE)
    
  }, error = function(e) {
    message("Password reset error: ", e$message)
    list(success = FALSE, message = "Something went wrong. Please try again.")
  })
  
  if (isTRUE(result$success)) {
    
    # Clear the form — use updateTextInput not updatePasswordInput
    updateTextInput(session, "reset_password",         value = "")
    updateTextInput(session, "reset_password_confirm", value = "")
    
    # Hide reset form
    shinyjs::runjs("Shiny.setInputValue('show_reset_form', false,
                    {priority: 'event'})")
    shinyjs::runjs("Shiny.setInputValue('url_reset_token', null,
                    {priority: 'event'})")
    
    output$reset_alert <- renderUI({ NULL })
    
    showNotification(
      "\u2713 Password updated successfully. Please sign in.",
      type     = "message",
      duration = 6
    )
    
  } else {
    output$reset_alert <- renderUI({
      div(class = "alert alert-danger",
          result$message %||% "Something went wrong.")
    })
  }
})

# ============================================
# Register — apply password strength validation
# ============================================

# Override register handler to add strength check
# (existing accounts not affected — only new registrations)
observeEvent(input$register_btn, {
  email            <- trimws(input$register_email %||% "")
  password         <- input$register_password %||% ""
  password_confirm <- input$register_password_confirm %||% ""
  
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
  
  # Password strength — only for new registrations
  strength_error <- validate_password_strength(password)
  if (!is.null(strength_error)) {
    rv$auth_message <- strength_error
    rv$auth_type    <- "danger"
    return()
  }
  
  login_id <- create_user(email, password)
  ensure_free_subscription(login_id)
  
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
    rv$is_demo             <- FALSE
    rv$demo_login_id       <- NULL
    rv$demo_targets        <- NULL
  } else {
    rv$auth_message <- "Email already exists"
    rv$auth_type    <- "danger"
  }
}, ignoreInit = TRUE, priority = 10)

