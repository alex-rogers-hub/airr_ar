# ============================================
# Account & Brand Management
# ============================================

# --- Reactives ---

user_subscription <- reactive({
  req(rv$logged_in, rv$login_id)
  get_user_subscription(rv$login_id)
})

user_main_brand <- reactive({
  req(rv$logged_in, rv$login_id)
  rv$brands_refresh
  get_user_main_brand(rv$login_id)
})

user_competitors <- reactive({
  req(rv$logged_in, rv$login_id)
  rv$brands_refresh
  get_user_competitor_brands_with_status(rv$login_id)
})

user_competitor_count <- reactive({
  req(rv$logged_in, rv$login_id)
  rv$brands_refresh
  get_user_competitor_count(rv$login_id)
})

user_tracked_queries <- reactive({
  req(rv$logged_in, rv$login_id)
  rv$queries_refresh
  get_user_tracked_queries(rv$login_id)
})

user_query_count <- reactive({
  req(rv$logged_in, rv$login_id)
  rv$queries_refresh
  get_user_query_count(rv$login_id)
})

# Auto-refresh for pending brands
observe({
  req(rv$logged_in, rv$login_id)
  competitors <- user_competitors()
  if (nrow(competitors) > 0 && any(!competitors$has_scores)) {
    invalidateLater(10000, session)
    # rv$brands_refresh <- isolate(rv$brands_refresh) + 1
  }
})

# ============================================
# Top Row Cards
# ============================================

# --- Profile Card ---
output$account_profile_card <- renderUI({
  req(rv$logged_in)
  sub <- user_subscription()
  
  tier_icon <- switch(sub$subscription_name,
                      "Free" = "seedling", "Pro" = "gem", "Enterprise" = "crown", "seedling")
  
  # Fetch main brand's industry + brand_id
  main_brand_info <- tryCatch(
    dbGetQuery(pool, "
      SELECT b.brand_id, b.brand_name, ubt.industry
      FROM fact_user_brands_tracked ubt
      JOIN dim_brand b ON b.brand_id = ubt.brand_id
      WHERE ubt.login_id = $1
        AND ubt.main_brand_flag = TRUE
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
      LIMIT 1",
               params = list(rv$login_id)),
    error = function(e) NULL
  )
  
  industry_str <- if (!is.null(main_brand_info) && nrow(main_brand_info) > 0 &&
                      !is.na(main_brand_info$industry[1]) &&
                      nzchar(main_brand_info$industry[1] %||% "")) {
    main_brand_info$industry[1]
  } else {
    "Not set"
  }
  
  edit_payload <- if (!is.null(main_brand_info) && nrow(main_brand_info) > 0) {
    jsonlite::toJSON(list(
      brand_id   = main_brand_info$brand_id[1],
      brand_name = main_brand_info$brand_name[1],
      industry   = if (is.na(main_brand_info$industry[1])) "" 
      else main_brand_info$industry[1]
    ), auto_unbox = TRUE)
  } else NULL
  
  div(
    class = "account-card account-card-profile",
    div(style = "font-size: 11px; text-transform: uppercase; letter-spacing: 1px; opacity: 0.8;",
        icon(tier_icon), " ", sub$subscription_name),
    div(style = "font-size: 24px; font-weight: 700; margin: 8px 0 4px;",
        rv$brand_name),
    div(style = "font-size: 13px; opacity: 0.8;",
        rv$email),
    
    # Industry row with edit link
    div(
      style = "margin-top: 10px; padding-top: 10px;
               border-top: 1px solid rgba(255,255,255,0.15);
               display: flex; align-items: center; gap: 8px;",
      icon("industry", style = "font-size: 11px; opacity: 0.7;"),
      tags$span(style = "font-size: 12px; opacity: 0.85;", industry_str),
      if (!is.null(edit_payload)) {
        tags$button(
          style = "background: rgba(255,255,255,0.15); border: 1px solid rgba(255,255,255,0.3);
                   border-radius: 5px; padding: 2px 8px; color: white; cursor: pointer;
                   font-size: 11px; font-weight: 600; transition: all 0.15s ease;",
          onclick = sprintf(
            "Shiny.setInputValue('edit_industry_btn', %s, {priority: 'event'})",
            edit_payload
          ),
          "Edit"
        )
      }
    )
  )
})

# --- SVG Gauge Helper ---
render_gauge <- function(used, total, color, label) {
  pct <- min(100, round(used / max(total, 1) * 100))
  radius <- 42
  circumference <- 2 * pi * radius
  dash_offset <- circumference * (1 - pct / 100)
  
  text_color <- if (pct >= 100) "#E74C3C" else if (pct >= 75) "#F39C12" else color
  
  div(
    class = "account-card account-card-gauge",
    div(
      class = "gauge-ring",
      HTML(sprintf('
        <svg width="100" height="100" viewBox="0 0 100 100">
          <circle cx="50" cy="50" r="%d" fill="none" stroke="#f0f0f0" stroke-width="8"/>
          <circle cx="50" cy="50" r="%d" fill="none" stroke="%s" stroke-width="8"
                  stroke-dasharray="%.1f" stroke-dashoffset="%.1f"
                  stroke-linecap="round" style="transition: stroke-dashoffset 0.8s ease;"/>
        </svg>',
                   radius, radius, text_color, circumference, dash_offset)),
      div(class = "gauge-text", style = paste0("color: ", text_color, ";"),
          paste0(used, "/", total))
    ),
    div(class = "gauge-label", label)
  )
}

# --- Brand Gauge ---
output$account_brand_gauge <- renderUI({
  req(user_subscription(), user_competitor_count())
  sub <- user_subscription()
  used <- user_competitor_count()
  max_comp <- sub$num_competitors_included + sub$extra_competitors_added
  render_gauge(used, max_comp, "#3498DB", "Competitor Slots")
})

# --- Query Gauge ---
output$account_query_gauge <- renderUI({
  req(user_subscription(), user_query_count())
  sub <- user_subscription()
  used <- user_query_count()
  max_q <- sub$num_prompts_included + sub$extra_prompts_added
  render_gauge(used, max_q, "#667eea", "Prompt Slots")
})

# --- Upgrade Card ---
output$account_upgrade_card <- renderUI({
  div(
    class = "account-card account-card-upgrade",
    icon("rocket", class = "fa-2x", style = "margin-bottom: 8px; opacity: 0.9;"),
    div(style = "font-size: 15px; font-weight: 600; margin-bottom: 4px;", "Need More?"),
    div(style = "font-size: 12px; opacity: 0.85; margin-bottom: 12px;",
        "Upgrade for more competitors & prompts"),
    actionButton("upgrade_btn", "View Plans", icon = icon("arrow-up"),
                 style = "background: rgba(255,255,255,0.2); border: 2px solid rgba(255,255,255,0.5);
                          color: white; border-radius: 8px; font-weight: 600; font-size: 13px;
                          padding: 6px 20px;")
  )
})

# ============================================
# Slot Badges (shown in list headers)
# ============================================

output$account_brand_slot_badge <- renderUI({
  req(user_subscription(), user_competitor_count())
  sub <- user_subscription()
  used <- user_competitor_count()
  max_comp <- sub$num_competitors_included + sub$extra_competitors_added
  remaining <- max_comp - used
  
  if (remaining > 0) {
    tags$span(class = "slot-badge available",
              paste0(remaining, " slot", ifelse(remaining != 1, "s", ""), " remaining"))
  } else {
    tags$span(class = "slot-badge full", "No slots remaining")
  }
})

output$account_query_slot_badge <- renderUI({
  req(user_subscription(), user_query_count())
  sub <- user_subscription()
  used <- user_query_count()
  max_q <- sub$num_prompts_included + sub$extra_prompts_added
  remaining <- max_q - used
  
  if (remaining > 0) {
    tags$span(class = "slot-badge available",
              paste0(remaining, " slot", ifelse(remaining != 1, "s", ""), " remaining"))
  } else {
    tags$span(class = "slot-badge full", "No slots remaining")
  }
})

# ── Low presence industry nudge ──────────────────────────────
output$account_industry_nudge <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  has_zero_presence <- tryCatch({
    result <- dbGetQuery(pool, "
      SELECT COALESCE(AVG(overall_score), -1) as avg_presence
      FROM fact_presence_history
      WHERE login_id = $1
        AND date >= CURRENT_DATE - INTERVAL '7 days'",
                         params = list(rv$login_id))
    score <- result$avg_presence[1]
    !is.na(score) && score >= 0 && score < 5
  }, error = function(e) FALSE)
  
  if (!has_zero_presence) return(NULL)
  
  # Get current industry
  current_industry <- tryCatch({
    dbGetQuery(pool, "
      SELECT industry FROM fact_user_brands_tracked
      WHERE login_id = $1
        AND main_brand_flag = TRUE
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)
      LIMIT 1",
               params = list(rv$login_id))$industry[1]
  }, error = function(e) NA)
  
  div(
    style = "background: rgba(231,76,60,0.06);
             border: 1px solid rgba(231,76,60,0.3);
             border-radius: 10px; padding: 14px 16px; margin-bottom: 20px;",
    div(
      style = "display: flex; align-items: flex-start; gap: 12px;",
      icon("triangle-exclamation",
           style = "color: #E74C3C; font-size: 18px; flex-shrink: 0; margin-top: 2px;"),
      div(
        style = "flex: 1;",
        div(
          style = "font-weight: 600; font-size: 14px; color: #C0392B; margin-bottom: 4px;",
          "Your Presence score is very low — your industry may be too broad"
        ),
        p(
          style = "font-size: 13px; color: #718096; margin: 0 0 10px; line-height: 1.5;",
          "AI models struggle to associate brands with broad categories. 
           A more specific industry description will significantly improve your scores."
        ),
        if (!is.na(current_industry) && nzchar(current_industry %||% "")) {
          div(
            style = "font-size: 12px; color: #718096; margin-bottom: 10px;",
            tags$span(style = "font-weight: 600;", "Current: "),
            tags$span(
              style = "background: rgba(231,76,60,0.1); color: #C0392B;
                       padding: 2px 8px; border-radius: 4px; font-weight: 500;",
              current_industry
            )
          )
        },
        div(
          style = "font-size: 12px; color: #718096; margin-bottom: 12px;",
          "Examples of good specific industries: ",
          tags$em('"Online Payment Infrastructure"'),
          ", ",
          tags$em('"Connected Fitness Hardware"'),
          ", ",
          tags$em('"Oat-based Dairy Alternatives"')
        ),
        # Inline edit trigger — reuses the existing modal
        tags$button(
          style = "background: #E74C3C; color: white; border: none;
                   border-radius: 8px; padding: 8px 18px; font-weight: 600;
                   font-size: 13px; cursor: pointer;",
          onclick = sprintf(
            "Shiny.setInputValue('edit_industry_btn', %s, {priority: 'event'})",
            jsonlite::toJSON(list(
              brand_name = rv$brand_name %||% "",
              industry   = current_industry %||% ""
            ), auto_unbox = TRUE)
          ),
          icon("pen-to-square", style = "margin-right: 6px;"),
          "Update Industry"
        )
      )
    )
  )
})

# ============================================
# Timing notice outputs — account page
# ============================================

output$account_competitor_timing_notice <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  # Only show when user has typed something
  brand_input <- input$add_competitor_brand_input
  if (is.null(brand_input) || nchar(trimws(brand_input)) < 2) return(NULL)
  
  est <- tryCatch(
    estimate_competitor_add_time(rv$login_id),
    error = function(e) NULL
  )
  if (is.null(est)) return(NULL)
  
  render_timing_notice(est$time_str, est$breakdown, "competitor")
})

output$account_prompt_timing_notice <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  # Only show when user has typed something
  prompt_input <- input$add_query_input
  if (is.null(prompt_input) || nchar(trimws(prompt_input)) < 2) return(NULL)
  
  est <- tryCatch(
    estimate_prompt_add_time(rv$login_id),
    error = function(e) NULL
  )
  if (is.null(est)) return(NULL)
  
  render_timing_notice(est$time_str, est$breakdown, "prompt")
})



# ============================================
# Competitor List
# ============================================

output$account_competitor_list <- renderUI({
  req(rv$logged_in)
  competitors <- user_competitors()
  
  if (nrow(competitors) == 0) {
    return(div(
      style = "text-align: center; padding: 30px;",
      icon("building", class = "fa-2x", style = "color: #e2e8f0; margin-bottom: 10px;"),
      p(style = "color: #a0aec0; font-size: 13px; margin: 0;",
        "No competitors added yet")
    ))
  }
  
  competitor_cards <- lapply(1:nrow(competitors), function(i) {
    bid        <- competitors$brand_id[i]
    bname      <- competitors$brand_name[i]
    date_added <- competitors$date_valid_from[i]
    has_scores <- competitors$has_scores[i]
    
    if (has_scores) {
      latest <- dbGetQuery(pool, "
        SELECT airr_score FROM fact_airr_history
        WHERE brand_id = $1 AND login_id = $2
        ORDER BY date DESC LIMIT 1",
                           params = list(bid, rv$login_id))
      
      score_text <- if (nrow(latest) > 0) {
        paste0("AiRR: ", round(latest$airr_score, 1))
      } else { "Active" }
      
      meta_html <- tags$span(
        tags$span(style = "color: #667eea; font-weight: 600;", score_text),
        tags$span(style = "margin: 0 6px; color: #e2e8f0;", "\u00B7"),
        format(date_added, "%b %d, %Y")
      )
      icon_class <- "account-list-icon brand"
      icon_name  <- "building"
    } else {
      meta_html <- tags$span(
        tags$i(class = "fa fa-spinner fa-spin", style = "margin-right: 4px;"),
        "Calculating scores..."
      )
      icon_class <- "account-list-icon brand pending"
      icon_name  <- "hourglass-half"
    }
    
    div(
      class = "account-list-item",
      div(class = icon_class, icon(icon_name)),
      div(class = "account-list-content",
          div(class = "name", bname),
          div(class = "meta", meta_html)),
      div(class = "account-list-actions",
          tags$button(
            class   = "btn-remove",
            onclick = sprintf(
              "Shiny.setInputValue('remove_brand_id', %d, {priority: 'event'})",
              bid
            ),
            icon("times")
          ))
    )
  })
  
  do.call(tagList, competitor_cards)
})

# ============================================
# Query List
# ============================================

output$account_query_list <- renderUI({
  req(rv$logged_in)
  queries <- user_tracked_queries()
  
  if (nrow(queries) == 0) {
    return(div(
      style = "text-align: center; padding: 30px;",
      icon("comment-dots", class = "fa-2x", style = "color: #e2e8f0; margin-bottom: 10px;"),
      p(style = "color: #a0aec0; font-size: 13px; margin: 0;", "No prompts tracked yet")
    ))
  }
  
  query_cards <- lapply(1:nrow(queries), function(i) {
    qid <- queries$query_id[i]
    qstring <- queries$query_string[i]
    date_added <- queries$date_valid_from[i]
    
    div(
      class = "account-list-item",
      div(class = "account-list-icon query", icon("comment-dots")),
      div(class = "account-list-content",
          div(class = "name", qstring),
          div(class = "meta", paste("Added:", format(date_added, "%b %d, %Y")))),
      div(class = "account-list-actions",
          tags$button(
            class = "btn-remove",
            onclick = sprintf("Shiny.setInputValue('remove_query_id', %d, {priority: 'event'})", qid),
            icon("times")
          ))
    )
  })
  
  do.call(tagList, query_cards)
})

# ============================================
# Add Competitor Handler
# ============================================

observeEvent(input$add_competitor_btn, {
  req(rv$logged_in, rv$login_id)
  brand_name <- trimws(input$add_competitor_brand_input)
  
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
    showNotification(
      paste0("You've used all ", max_competitors, " competitor slots. Upgrade to add more!"),
      type = "error", duration = 5)
    return()
  }
  
  # Get industry from main brand
  user_industry <- dbGetQuery(pool, "
    SELECT ubt.industry
    FROM fact_user_brands_tracked ubt
    WHERE ubt.login_id = $1
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LIMIT 1",
                              params = list(rv$login_id))
  
  industry <- if (nrow(user_industry) > 0 && !is.na(user_industry$industry[1])) {
    user_industry$industry[1]
  } else {
    NULL
  }
  
  brand_result <- add_brand_for_user_pending(rv$login_id, brand_name,
                                             main_brand = FALSE,
                                             industry = industry)
  
  if (is.null(brand_result)) {
    showNotification("Error adding brand. Please try again.", type = "error", duration = 3)
    return()
  }
  
  link_existing_queries_to_brand(rv$login_id, brand_result$brand_id)
  
  updateTextInput(session, "add_competitor_brand_input", value = "")
  rv$brands_refresh <- rv$brands_refresh + 1
  
  # Get user's tracked prompts for the job payload
  user_queries <- get_user_tracked_queries(rv$login_id)
  prompt_map <- if (nrow(user_queries) > 0) {
    setNames(as.list(user_queries$query_id), user_queries$query_string)
  } else {
    list()
  }
  
  # Insert job into queue
  dbExecute(pool, "
    INSERT INTO dim_job_queue (job_type, login_id, payload)
    VALUES ('score_competitor', $1, $2)",
            params = list(rv$login_id, toJSON(list(
              brand_name = brand_name,
              brand_id   = brand_result$brand_id,
              prompts    = prompt_map
            ), auto_unbox = TRUE)))
  
  showNotification(
    paste0("\u2713 ", brand_name, " added! Scores calculating in background."),
    type = "message", duration = 5)
  
  NULL
})

# ============================================
# Add Query Handler
# ============================================

observeEvent(input$add_query_btn, {
  req(rv$logged_in, rv$login_id)
  query_string <- trimws(input$add_query_input)
  
  if (query_string == "") {
    showNotification("Please enter a prompt.", type = "error", duration = 3)
    return()
  }
  
  existing <- user_tracked_queries()
  if (nrow(existing) > 0 && tolower(query_string) %in% tolower(existing$query_string)) {
    showNotification("You're already tracking this prompt.", type = "warning", duration = 3)
    return()
  }
  
  sub <- user_subscription()
  used <- user_query_count()
  max_queries <- sub$num_prompts_included + sub$extra_prompts_added
  
  if (used >= max_queries) {
    showNotification(
      paste0("You've used all ", max_queries, " prompt slots. Upgrade to add more!"),
      type = "error", duration = 5)
    return()
  }
  
  query_result <- add_query_for_user(rv$login_id, query_string)
  
  if (is.null(query_result)) {
    showNotification("Error adding prompt. Please try again.", type = "error", duration = 3)
    return()
  }
  
  updateTextInput(session, "add_query_input", value = "")
  rv$queries_refresh <- rv$queries_refresh + 1
  
  # Get all user's brand IDs for the job payload
  user_brands <- get_user_brands(rv$login_id)
  brand_ids <- user_brands$brand_id
  
  # Insert job into queue
  dbExecute(pool, "
    INSERT INTO dim_job_queue (job_type, login_id, payload)
    VALUES ('score_query', $1, $2)",
            params = list(rv$login_id, toJSON(list(
              query_string = query_string,
              query_id     = query_result$query_id,
              brand_ids    = brand_ids
            ), auto_unbox = TRUE)))
  
  showNotification(
    paste0("\u2713 Prompt added! Scores calculating in background."),
    type = "message", duration = 5)
  
  NULL
})

# ============================================
# Remove Handlers
# ============================================

observeEvent(input$remove_brand_id, {
  req(rv$logged_in, rv$login_id)
  brand_id <- input$remove_brand_id
  brand_info <- dbGetQuery(pool,
                           "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                           params = list(brand_id))
  
  success <- remove_brand_for_user(rv$login_id, brand_id)
  if (success) {
    showNotification(paste0("\u2713 ", brand_info$brand_name, " removed"),
                     type = "message", duration = 3)
    rv$brands_refresh <- rv$brands_refresh + 1
  } else {
    showNotification("Error removing brand", type = "error", duration = 3)
  }
})

observeEvent(input$remove_query_id, {
  req(rv$logged_in, rv$login_id)
  query_id <- input$remove_query_id
  
  success <- remove_query_for_user(rv$login_id, query_id)
  if (success) {
    showNotification("\u2713 Prompt removed", type = "message", duration = 3)
    rv$queries_refresh <- rv$queries_refresh + 1
  } else {
    showNotification("Error removing prompt", type = "error", duration = 3)
  }
})

# ============================================
# Upgrade Modal
# ============================================

observeEvent(input$upgrade_btn, {
  showModal(modalDialog(
    title = div(style = "font-weight: 600;", "Subscription Plans"),
    size = "l",
    div(
      style = "display: flex; gap: 15px;",
      
      # Core
      div(
        style = "flex: 1; text-align: center; padding: 25px 15px; border: 2px solid #ecf0f1; 
                 border-radius: 12px;",
        div(style = "color: #95a5a6;", icon("seedling", class = "fa-2x")),
        h4(style = "margin: 10px 0 0;", "Core"),
        h3(style = "color: #27AE60; margin: 5px 0;", "TBD"),
        p(style = "color: #a0aec0; font-size: 12px;", "per month"),
        hr(style = "border-color: #f0f0f0;"),
        div(style = "font-size: 13px; text-align: left; padding: 0 10px;",
            p("\u2713 1 competitor"),
            p("\u2713 1 tracked prompt"),
            p("\u2717 Priority processing"))
      ),
      
      # Pro
      div(
        style = "flex: 1; text-align: center; padding: 25px 15px; border: 2px solid #3498DB; 
                 border-radius: 12px; box-shadow: 0 4px 15px rgba(52,152,219,0.15);
                 position: relative;",
        tags$span(
          style = "position: absolute; top: -12px; left: 50%; transform: translateX(-50%);
                   background: #3498DB; color: white; font-size: 11px; font-weight: 600;
                   padding: 3px 12px; border-radius: 20px;",
          "POPULAR"
        ),
        div(style = "color: #3498DB;", icon("gem", class = "fa-2x")),
        h4(style = "margin: 10px 0 0;", "Pro"),
        h3(style = "color: #3498DB; margin: 5px 0;", "TBD"),
        p(style = "color: #a0aec0; font-size: 12px;", "per month"),
        hr(style = "border-color: #f0f0f0;"),
        div(style = "font-size: 13px; text-align: left; padding: 0 10px;",
            p(strong("\u2713 3 competitors")),
            p(strong("\u2713 5 tracked prompts")),
            p("\u2713 Priority processing")),
        br(),
        actionButton("select_pro_btn", "Select Pro", class = "btn-primary",
                     style = "width: 100%; border-radius: 8px;")
      ),
      
      # Enterprise
      div(
        style = "flex: 1; text-align: center; padding: 25px 15px; border: 2px solid #F39C12; 
                 border-radius: 12px;",
        div(style = "color: #F39C12;", icon("crown", class = "fa-2x")),
        h4(style = "margin: 10px 0 0;", "Enterprise"),
        h3(style = "color: #F39C12; margin: 5px 0;", "TBD"),
        p(style = "color: #a0aec0; font-size: 12px;", "per month"),
        hr(style = "border-color: #f0f0f0;"),
        div(style = "font-size: 13px; text-align: left; padding: 0 10px;",
            p(strong("\u2713 10 competitors")),
            p(strong("\u2713 20 tracked prompts")),
            p("\u2713 Priority processing"),
            p(strong("\u2713 Customer Personas")),
            p(strong("\u2713 API access"))),
        br(),
        actionButton("select_enterprise_btn", "Contact Sales", class = "btn-warning",
                     style = "width: 100%; border-radius: 8px;")
      )
    ),
    
    div(
      style = "text-align: center; margin-top: 20px; padding: 12px;
               background: #f8f9fa; border-radius: 8px;",
      tags$span(
        style = "font-size: 12px; color: #a0aec0;",
        icon("info-circle", style = "margin-right: 4px;"),
        "Pricing is being finalised. Get in touch to discuss early access rates."
      )
    ),
    
    footer = modalButton("Close"),
    easyClose = TRUE
  ))
})

