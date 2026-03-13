# ============================================
# Modal handlers for Add Competitor / Add Prompt
# ============================================

# --- Add Competitor modal (from Brand Overview) ---
observeEvent(input$add_competitor_from_brand, {
  req(rv$logged_in)
  
  sub  <- user_subscription()
  used <- user_competitor_count()
  max_competitors <- sub$num_competitors_included + sub$extra_competitors_added
  
  # Pre-calculate timing estimate for modal
  est <- tryCatch(
    estimate_competitor_add_time(rv$login_id),
    error = function(e) NULL
  )
  
  showModal(modalDialog(
    title = div(
      icon("plus-circle", style = "color: #667eea;"),
      " Add Competitor Brand"
    ),
    size = "m",
    
    div(
      style = "padding: 10px;",
      p(style = "color: #7f8c8d; margin-bottom: 15px;",
        sprintf("You're using %d of %d competitor slots.",
                as.integer(used), as.integer(max_competitors))),
      
      if (used >= max_competitors) {
        div(
          style = "text-align: center; padding: 20px;",
          icon("exclamation-triangle", class = "fa-2x",
               style = "color: #E74C3C; margin-bottom: 10px;"),
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
          
          # Timing notice
          if (!is.null(est)) {
            render_timing_notice(est$time_str, est$breakdown, "competitor")
          },
          
          div(
            style = "margin-top: 15px;",
            actionButton(
              "modal_add_competitor_btn",
              "Add Competitor",
              icon  = icon("plus"),
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

# --- Handle competitor submission ---
observeEvent(input$modal_add_competitor_btn, {
  req(rv$logged_in, rv$login_id)
  
  brand_name <- trimws(input$modal_competitor_name)
  
  if (brand_name == "") {
    showNotification("Please enter a brand name.", type = "error", duration = 3)
    return()
  }
  
  # Duplicate checks
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
  
  # Slot check
  sub  <- user_subscription()
  used <- user_competitor_count()
  max_competitors <- sub$num_competitors_included + sub$extra_competitors_added
  
  if (used >= max_competitors) {
    showNotification("No slots available. Upgrade to add more!",
                     type = "error", duration = 5)
    return()
  }
  
  # Get user's industry from main brand
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
  
  # Add brand to DB (pending scores)
  brand_result <- add_brand_for_user_pending(
    rv$login_id, brand_name,
    main_brand = FALSE,
    industry   = industry
  )
  
  if (is.null(brand_result)) {
    showNotification("Error adding brand. Please try again.",
                     type = "error", duration = 3)
    return()
  }
  
  # Link user's existing queries to the new competitor
  link_existing_queries_to_brand(rv$login_id, brand_result$brand_id)
  
  # Build prompt map for job payload
  user_queries <- get_user_tracked_queries(rv$login_id)
  prompt_map <- if (nrow(user_queries) > 0) {
    setNames(as.list(user_queries$query_id), user_queries$query_string)
  } else {
    list()
  }
  
  # Queue scoring job on compute server — does NOT block the UI
  dbExecute(pool, "
    INSERT INTO dim_job_queue (job_type, login_id, payload)
    VALUES ('score_competitor', $1, $2)",
            params = list(
              rv$login_id,
              toJSON(list(
                brand_name = brand_name,
                brand_id   = brand_result$brand_id,
                prompts    = prompt_map
              ), auto_unbox = TRUE)
            ))
  
  # Close modal
  removeModal()
  
  # Refresh brand list — existing scores stay visible,
  # new brand shows spinner until worker completes
  rv$brands_refresh <- rv$brands_refresh + 1
  
  showNotification(
    paste0("\u2713 ", brand_name,
           " added! Scores calculating in the background \u2014 ",
           "your existing data is unaffected."),
    type     = "message",
    duration = 6
  )
})

# --- Upgrade from modal ---
observeEvent(input$upgrade_from_modal, {
  removeModal()
  shinyjs::click("upgrade_btn")
})

# ============================================
# Add Prompt modal (from Prompt Overview)
# ============================================

observeEvent(input$add_prompt_from_overview, {
  req(rv$logged_in)
  
  sub  <- user_subscription()
  used <- user_query_count()
  max_queries <- sub$num_prompts_included + sub$extra_prompts_added
  
  # Pre-calculate timing estimate for modal
  est <- tryCatch(
    estimate_prompt_add_time(rv$login_id),
    error = function(e) NULL
  )
  
  showModal(modalDialog(
    title = div(
      icon("comment-dots", style = "color: #667eea;"),
      " Add New Prompt"
    ),
    size = "m",
    
    div(
      style = "padding: 10px;",
      
      if (used >= max_queries) {
        div(
          style = "text-align: center; padding: 20px;",
          icon("exclamation-triangle", class = "fa-2x",
               style = "color: #E74C3C; margin-bottom: 10px;"),
          h4("All prompt slots used"),
          p(sprintf("You're using %d of %d prompt slots.",
                    as.integer(used), as.integer(max_queries))),
          p("Upgrade your subscription to add more prompts."),
          actionButton("upgrade_from_modal_prompt", "View Plans",
                       icon = icon("arrow-up"), class = "btn-success")
        )
      } else {
        tagList(
          p(style = "color: #7f8c8d; margin-bottom: 15px;",
            sprintf("You're using %d of %d prompt slots.",
                    as.integer(used), as.integer(max_queries))),
          textInput(
            "modal_prompt_text",
            "Prompt:",
            placeholder = "e.g. What are the best running shoes?",
            width = "100%"
          ),
          
          # Timing notice
          if (!is.null(est)) {
            render_timing_notice(est$time_str, est$breakdown, "prompt")
          },
          
          div(
            style = "margin-top: 15px;",
            actionButton(
              "modal_add_prompt_btn",
              "Add Prompt",
              icon  = icon("plus"),
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

# --- Handle prompt submission ---
observeEvent(input$modal_add_prompt_btn, {
  req(rv$logged_in, rv$login_id)
  
  prompt_text <- trimws(input$modal_prompt_text)
  
  if (prompt_text == "") {
    showNotification("Please enter a prompt.", type = "error", duration = 3)
    return()
  }
  
  # Duplicate check
  existing <- user_tracked_queries()
  if (nrow(existing) > 0 &&
      tolower(prompt_text) %in% tolower(existing$query_string)) {
    showNotification("You're already tracking this prompt.",
                     type = "warning", duration = 3)
    return()
  }
  
  # Slot check
  sub  <- user_subscription()
  used <- user_query_count()
  max_queries <- sub$num_prompts_included + sub$extra_prompts_added
  
  if (used >= max_queries) {
    showNotification("No prompt slots available. Upgrade to add more!",
                     type = "error", duration = 5)
    return()
  }
  
  # Add query to DB and link to all user brands
  query_result <- add_query_for_user(rv$login_id, prompt_text)
  
  if (is.null(query_result)) {
    showNotification("Error adding prompt. Please try again.",
                     type = "error", duration = 3)
    return()
  }
  
  # Get all user brand IDs for job payload
  user_brands <- get_user_brands(rv$login_id)
  brand_ids   <- user_brands$brand_id
  
  # Queue scoring job on compute server
  dbExecute(pool, "
    INSERT INTO dim_job_queue (job_type, login_id, payload)
    VALUES ('score_query', $1, $2)",
            params = list(
              rv$login_id,
              toJSON(list(
                query_string = prompt_text,
                query_id     = query_result$query_id,
                brand_ids    = brand_ids
              ), auto_unbox = TRUE)
            ))
  
  # Close modal and refresh
  removeModal()
  rv$queries_refresh <- rv$queries_refresh + 1
  
  showNotification(
    paste0("\u2713 Prompt added! Scores calculating in the background \u2014 ",
           "your existing data is unaffected."),
    type     = "message",
    duration = 6
  )
})

# --- Upgrade from prompt modal ---
observeEvent(input$upgrade_from_modal_prompt, {
  removeModal()
  shinyjs::click("upgrade_btn")
})

# ============================================
# Edit Industry modal
# ============================================

industry_edit_payload <- reactiveVal(NULL)

# Step 1: catch the button click, push payload into reactiveVal
observeEvent(input$edit_industry_btn, {
  req(rv$logged_in, rv$login_id)
  cat("=== edit_industry_btn fired ===\n")
  
  payload <- input$edit_industry_btn
  
  if (is.null(payload$brand_name)) {
    cat("Payload missing brand_name, aborting\n")
    return()
  }
  
  cat("Brand name:", payload$brand_name, "\n")
  cat("Industry:", payload$industry %||% "(none)", "\n")
  
  industry_edit_payload(payload)
})

# Step 2: open the modal when the reactiveVal updates
observeEvent(industry_edit_payload(), {
  payload <- industry_edit_payload()
  if (is.null(payload)) return()
  
  cat("=== Opening edit industry modal ===\n")
  
  # Pre-calculate rescore time to show in modal
  est <- tryCatch(
    estimate_rescore_time(rv$login_id),
    error = function(e) NULL
  )
  
  rescore_notice <- if (!is.null(est)) {
    div(
      style = "background: rgba(212,168,67,0.08);
               border: 1px solid rgba(212,168,67,0.3);
               border-radius: 8px; padding: 10px 14px; margin-top: 16px;",
      div(
        style = "display: flex; align-items: center; gap: 7px; margin-bottom: 6px;",
        icon("rotate", style = "color: #D4A843; font-size: 13px; flex-shrink: 0;"),
        tags$span(
          style = "font-size: 13px; font-weight: 600; color: #2d3748;",
          paste0("Scores will update in ", est$time_str)
        )
      ),
      # Breakdown rows
      div(
        style = "display: flex; flex-direction: column; gap: 3px;",
        div(
          style = "display: flex; justify-content: space-between;
                   font-size: 11px; color: #718096;",
          tags$span(paste0(est$n_brands, " brand",
                           if (est$n_brands != 1) "s" else "", " to rescore")),
          tags$span(paste0("~", round(est$breakdown$brands / 60), " min"))
        ),
        if (est$n_prompts > 0) {
          div(
            style = "display: flex; justify-content: space-between;
                     font-size: 11px; color: #718096;",
            tags$span(paste0(est$n_prompts, " prompt",
                             if (est$n_prompts != 1) "s" else "",
                             " \u00d7 ", est$n_brands, " brand",
                             if (est$n_brands != 1) "s" else "")),
            tags$span(paste0("~", round(est$breakdown$prompts / 60), " min"))
          )
        },
        if (est$n_personas > 0) {
          div(
            style = "display: flex; justify-content: space-between;
                     font-size: 11px; color: #718096;",
            tags$span(paste0(est$n_personas, " persona",
                             if (est$n_personas != 1) "s" else "",
                             " \u00d7 ", est$n_brands, " brand",
                             if (est$n_brands != 1) "s" else "")),
            tags$span(paste0("~", round(est$breakdown$persona / 60), " min"))
          )
        },
        div(style = "height: 1px; background: rgba(212,168,67,0.2); margin: 4px 0;"),
        div(
          style = "display: flex; justify-content: space-between;
                   font-size: 12px; font-weight: 600; color: #D4A843;",
          tags$span("Total"),
          tags$span(est$time_str)
        )
      ),
      div(
        style = "margin-top: 8px; font-size: 11px; color: #a0aec0;",
        icon("info-circle", style = "margin-right: 3px;"),
        "Rescoring runs in the background — your current scores stay visible
         until new ones are ready."
      )
    )
  } else NULL
  
  showModal(modalDialog(
    title = div(
      icon("industry", style = "color: #667eea;"),
      " Edit Industry"
    ),
    size = "s",
    easyClose = TRUE,
    
    div(
      style = "padding: 10px;",
      
      p(style = "color: #718096; font-size: 13px; margin-bottom: 6px;",
        "This will update the industry for ", tags$strong("all brands"),
        " on your account, including competitors."),
      
      p(style = "color: #a0aec0; font-size: 12px; margin-bottom: 16px;",
        "Be specific — ",
        tags$em('"Athletic Footwear"'), " is better than ",
        tags$em('"Retail"'), "."),
      
      textInput(
        "modal_industry_input",
        "Industry",
        value       = payload$industry %||% "",
        placeholder = "e.g. Athletic Footwear",
        width       = "100%"
      ),
      
      # Quick suggestions
      div(
        style = "margin-top: 10px;",
        tags$label(
          style = "font-size: 11px; color: #a0aec0; text-transform: uppercase;
                   letter-spacing: 0.5px; font-weight: 600;",
          "Quick suggestions"
        ),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 6px; margin-top: 6px;",
          lapply(
            c("SaaS", "E-commerce", "Financial Services", "Healthcare",
              "Hospitality", "Real Estate", "Legal Services", "Consulting",
              "Consumer Electronics", "Food & Beverage"),
            function(sugg) {
              tags$span(
                style = "padding: 4px 10px; border-radius: 12px; font-size: 12px;
                         background: #f7f7f7; border: 1px solid #e2e8f0;
                         cursor: pointer; color: #4a5568;
                         transition: all 0.15s ease;",
                onclick = sprintf(
                  "document.getElementById('modal_industry_input').value = '%s';
                   Shiny.setInputValue('modal_industry_input', '%s', {priority: 'event'});",
                  sugg, sugg
                ),
                sugg
              )
            }
          )
        )
      ),
      
      # Rescore timing notice
      rescore_notice
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        "modal_save_industry_btn",
        "Save & Rescore",
        icon  = icon("rotate"),
        class = "btn-primary"
      )
    )
  ))
})

# Step 3: save and queue rescore jobs
observeEvent(input$modal_save_industry_btn, {
  req(rv$logged_in, rv$login_id)
  
  industry <- trimws(input$modal_industry_input)
  
  if (nchar(industry) < 2) {
    showNotification("Please enter an industry.", type = "error", duration = 3)
    return()
  }
  
  result <- update_user_brand_industry(rv$login_id, industry)
  
  if (!result$success) {
    showNotification("Error saving industry. Please try again.",
                     type = "error", duration = 3)
    return()
  }
  
  brand_ids <- result$brand_ids
  
  # Get user's tracked queries for prompt rescoring
  user_queries <- get_user_tracked_queries(rv$login_id)
  prompt_map <- if (nrow(user_queries) > 0) {
    setNames(as.list(user_queries$query_id), user_queries$query_string)
  } else list()
  
  # Queue a rescore job for each brand
  for (bid in brand_ids) {
    brand_meta <- dbGetQuery(pool,
                             "SELECT brand_name, main_brand_flag
       FROM dim_brand b
       JOIN fact_user_brands_tracked ubt ON ubt.brand_id = b.brand_id
       WHERE b.brand_id = $1 AND ubt.login_id = $2
       LIMIT 1",
                             params = list(bid, rv$login_id))
    
    if (nrow(brand_meta) == 0) next
    
    is_main <- isTRUE(brand_meta$main_brand_flag[1])
    
    dbExecute(pool, "
      INSERT INTO dim_job_queue (job_type, login_id, payload)
      VALUES ($1, $2, $3)",
              params = list(
                if (is_main) "score_brand" else "score_competitor",
                rv$login_id,
                toJSON(list(
                  brand_name = brand_meta$brand_name[1],
                  brand_id   = bid,
                  prompts    = prompt_map
                ), auto_unbox = TRUE)
              ))
  }
  
  # Queue persona rescoring if any exist
  persona_ids <- tryCatch(
    dbGetQuery(pool, "
      SELECT profile_id FROM fact_user_profiles_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
               params = list(rv$login_id))$profile_id,
    error = function(e) c()
  )
  
  for (pid in persona_ids) {
    dbExecute(pool, "
      INSERT INTO dim_job_queue (job_type, login_id, payload)
      VALUES ('score_profile', $1, $2)",
              params = list(
                rv$login_id,
                toJSON(list(profile_id = pid), auto_unbox = TRUE)
              ))
  }
  
  removeModal()
  industry_edit_payload(NULL)
  rv$brands_refresh <- rv$brands_refresh + 1
  
  n_jobs <- length(brand_ids) + length(persona_ids)
  
  showNotification(
    paste0("\u2713 Industry updated to \"", industry, "\" \u2014 ",
           n_jobs, " rescore job", if (n_jobs != 1) "s" else "",
           " queued. Your dashboard will update automatically."),
    type     = "message",
    duration = 8
  )
})

