# ============================================
# Modal handlers for Add Competitor / Add Prompt
# ============================================

# --- Add Competitor modal (from Brand Overview) ---
observeEvent(input$add_competitor_from_brand, {
  req(rv$logged_in)
  
  sub <- user_subscription()
  used <- user_competitor_count()
  max_competitors <- sub$num_competitors_included + sub$extra_competitors_added
  
  showModal(modalDialog(
    title = div(
      icon("plus-circle", style = "color: #667eea;"),
      " Add Competitor Brand"
    ),
    size = "m",
    
    div(
      style = "padding: 10px;",
      p(style = "color: #7f8c8d; margin-bottom: 15px;",
        sprintf("You're using %d of %d competitor slots.", used, max_competitors)),
      
      if (used >= max_competitors) {
        div(
          style = "text-align: center; padding: 20px;",
          icon("exclamation-triangle", class = "fa-2x", style = "color: #E74C3C; margin-bottom: 10px;"),
          h4("All slots used"),
          p("Upgrade your subscription to add more competitors."),
          actionButton("upgrade_from_modal", "View Plans",
                       icon = icon("arrow-up"), class = "btn-success")
        )
      } else {
        tagList(
          textInput(
            "modal_competitor_name",
            "Brand Name:",
            placeholder = "Enter competitor brand name...",
            width = "100%"
          ),
          div(
            style = "margin-top: 15px;",
            actionButton(
              "modal_add_competitor_btn",
              "Add Competitor",
              icon = icon("plus"),
              class = "btn-primary",
              style = "width: 100%; padding: 10px; font-weight: 600;"
            )
          )
        )
      }
    ),
    
    footer = modalButton("Close"),
    easyClose = TRUE
  ))
})

# Handle competitor submission from modal
observeEvent(input$modal_add_competitor_btn, {
  req(rv$logged_in, rv$login_id)
  
  brand_name <- trimws(input$modal_competitor_name)
  
  if (brand_name == "") {
    showNotification("Please enter a brand name.", type = "error", duration = 3)
    return()
  }
  
  main <- user_main_brand()
  if (nrow(main) > 0 && tolower(brand_name) == tolower(main$brand_name)) {
    showNotification("This is already your main brand!", type = "warning", duration = 3)
    return()
  }
  
  existing <- user_competitors()
  if (nrow(existing) > 0 && tolower(brand_name) %in% tolower(existing$brand_name)) {
    showNotification("You're already tracking this brand.", type = "warning", duration = 3)
    return()
  }
  
  sub <- user_subscription()
  used <- user_competitor_count()
  max_competitors <- sub$num_competitors_included + sub$extra_competitors_added
  
  if (used >= max_competitors) {
    showNotification("No slots available. Upgrade to add more!", type = "error", duration = 5)
    return()
  }
  
  # NEW: pull user's industry from their main brand tracking record
  user_industry <- dbGetQuery(pool, "
    SELECT ubt.industry
    FROM fact_user_brands_tracked ubt
    WHERE ubt.login_id = $1
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LIMIT 1
  ", params = list(rv$login_id))
  
  industry <- if (nrow(user_industry) > 0 && !is.na(user_industry$industry[1])) {
    user_industry$industry[1]
  } else {
    NULL
  }
  
  # CHANGED: pass industry
  brand_result <- add_brand_for_user_pending(rv$login_id, brand_name, 
                                             main_brand = FALSE,
                                             industry = industry)
  
  if (is.null(brand_result)) {
    showNotification("Error adding brand.", type = "error", duration = 3)
    return()
  }
  
  removeModal()
  rv$brands_refresh <- rv$brands_refresh + 1
  
  if (brand_result$needs_scoring) {
    showNotification(
      paste0("✓ ", brand_name, " added! Calculating scores in the background."),
      type = "message", duration = 5
    )
    
    brand_name_copy <- brand_name
    login_id_copy   <- rv$login_id
    
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
        # CHANGED: pass login_id
        user_create_airr(brand_name_copy, login_id_copy)
        list(success = TRUE, brand = brand_name_copy)
      }, error = function(e) {
        list(success = FALSE, brand = brand_name_copy, error = e$message)
      })
    }) %...>% (function(result) {
      if (result$success) {
        showNotification(paste0("✓ Scores ready for ", result$brand, "!"),
                         type = "message", duration = 5)
      } else {
        showNotification(paste0("⚠ Scoring failed for ", result$brand),
                         type = "warning", duration = 10)
      }
      rv$brands_refresh <- rv$brands_refresh + 1
    }) %...!% (function(err) {
      showNotification(paste0("⚠ Error: ", err$message), type = "error", duration = 10)
      rv$brands_refresh <- rv$brands_refresh + 1
    })
  } else {
    showNotification(paste0("✓ ", brand_name, " added! Scores already available."),
                     type = "message", duration = 3)
  }
})

# Upgrade from modal
observeEvent(input$upgrade_from_modal, {
  removeModal()
  # Trigger the main upgrade modal
  shinyjs::click("upgrade_btn")
})

# --- Add Prompt modal (from Prompt Overview) ---
observeEvent(input$add_prompt_from_overview, {
  req(rv$logged_in)
  
  showModal(modalDialog(
    title = div(
      icon("comment-dots", style = "color: #667eea;"),
      " Add New Prompt"
    ),
    size = "m",
    
    div(
      style = "padding: 10px;",
      p(style = "color: #7f8c8d; margin-bottom: 15px;",
        "Add a prompt to track how your brand and competitors appear in AI responses."),
      
      textInput(
        "modal_prompt_text",
        "Prompt:",
        placeholder = "e.g. What are the best running shoes?",
        width = "100%"
      ),
      
      div(
        style = "margin-top: 15px;",
        actionButton(
          "modal_add_prompt_btn",
          "Add Prompt",
          icon = icon("plus"),
          class = "btn-primary",
          style = "width: 100%; padding: 10px; font-weight: 600;"
        )
      )
    ),
    
    footer = modalButton("Close"),
    easyClose = TRUE
  ))
})

# Handle prompt submission from modal
observeEvent(input$modal_add_prompt_btn, {
  req(rv$logged_in, rv$brand_id)
  
  prompt_text <- trimws(input$modal_prompt_text)
  
  if (prompt_text == "") {
    showNotification("Please enter a prompt.", type = "error", duration = 3)
    return()
  }
  
  removeModal()
  
  showNotification(
    paste0("Adding prompt and calculating scores..."),
    type = "message", duration = NULL, id = "prompt_adding"
  )
  
  # Add prompt for the user's brand
  result <- tryCatch({
    query_id <- addPrompt(prompt_text, rv$brand_id)
    list(success = TRUE, query_id = query_id)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  removeNotification(id = "prompt_adding")
  
  if (result$success) {
    showNotification(
      paste0("✓ Prompt added successfully!"),
      type = "message", duration = 3
    )
    
    # Refresh the query dropdown
    rv$queries_refresh <- rv$queries_refresh + 1
    
    # Update the dashboard query selector
    brands <- user_all_brand_ids()
    if (!is.null(brands) && nrow(brands) > 0) {
      brand_ids <- brands$brand_id
      placeholders <- paste0("$", seq_along(brand_ids), collapse = ", ")
      
      queries <- dbGetQuery(pool, sprintf("
        SELECT DISTINCT dq.query_id, dq.query_string
        FROM dim_brand_query dbq
        JOIN dim_query dq ON dbq.query_id = dq.query_id
        WHERE dbq.brand_id IN (%s)
        ORDER BY dq.query_string
      ", placeholders), params = as.list(brand_ids))
      
      query_choices <- setNames(queries$query_string, queries$query_string)
      
      updateSelectInput(session, "dash_query_select",
                        choices = c("Select a query..." = "", query_choices),
                        selected = prompt_text)
    }
  } else {
    showNotification(
      paste0("Error: ", result$error),
      type = "error", duration = 5
    )
  }
})