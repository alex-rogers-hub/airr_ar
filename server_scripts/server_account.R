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

user_persona_count <- reactive({
  req(rv$logged_in, rv$login_id)
  profiles_refresh()
  tryCatch(
    dbGetQuery(pool, "
      SELECT COUNT(*) as cnt
      FROM fact_user_profiles_tracked
      WHERE login_id = $1
        AND date_valid_from <= CURRENT_DATE
        AND (date_valid_to IS NULL OR date_valid_to > CURRENT_DATE)",
               params = list(rv$login_id))$cnt,
    error = function(e) 0
  )
})

user_reach_info <- reactive({
  req(rv$logged_in, rv$login_id)
  rv$brands_refresh
  tryCatch(
    dbGetQuery(pool, "
      SELECT b.brand_reach, b.reach_country, b.reach_region, b.reach_postcode
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
})

# Alias refresh trigger
alias_refresh <- reactiveVal(0)

# Auto-refresh for pending brands
observe({
  req(rv$logged_in, rv$login_id)
  competitors <- user_competitors()
  if (nrow(competitors) > 0 && any(!competitors$has_scores)) {
    invalidateLater(10000, session)
  }
})

# ============================================
# Top Row Cards
# ============================================

output$account_profile_card <- renderUI({
  req(rv$logged_in)
  sub <- user_subscription()
  
  tier_icon <- switch(sub$subscription_name,
                      "Free" = "seedling", "Starter" = "seedling",
                      "Pro" = "gem", "Enterprise" = "crown", "seedling")
  
  main_brand_info <- tryCatch(
    dbGetQuery(pool, "
      SELECT b.brand_id, b.brand_name, b.brand_reach, b.reach_country,
             b.reach_region, b.reach_postcode, ubt.industry
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
  } else "Not set"
  
  reach_str <- if (!is.null(main_brand_info) && nrow(main_brand_info) > 0) {
    format_reach_display(
      main_brand_info$brand_reach[1],
      main_brand_info$reach_country[1],
      main_brand_info$reach_region[1],
      main_brand_info$reach_postcode[1]
    )
  } else "Global"
  
  industry_payload <- if (!is.null(main_brand_info) && nrow(main_brand_info) > 0) {
    jsonlite::toJSON(list(
      brand_id   = main_brand_info$brand_id[1],
      brand_name = main_brand_info$brand_name[1],
      industry   = if (is.na(main_brand_info$industry[1])) ""
      else main_brand_info$industry[1]
    ), auto_unbox = TRUE)
  } else NULL
  
  reach_payload <- if (!is.null(main_brand_info) && nrow(main_brand_info) > 0) {
    jsonlite::toJSON(list(
      brand_reach    = main_brand_info$brand_reach[1]    %||% "global",
      reach_country  = main_brand_info$reach_country[1]  %||% "",
      reach_region   = main_brand_info$reach_region[1]   %||% "",
      reach_postcode = main_brand_info$reach_postcode[1] %||% ""
    ), auto_unbox = TRUE)
  } else NULL
  
  div(
    class = "account-card account-card-profile",
    div(style = "font-size: 11px; text-transform: uppercase;
                 letter-spacing: 1px; opacity: 0.8;",
        icon(tier_icon), " ", sub$subscription_name),
    div(style = "font-size: 24px; font-weight: 700; margin: 8px 0 4px;",
        rv$brand_name),
    div(style = "font-size: 13px; opacity: 0.8;", rv$email),
    
    div(
      style = "margin-top: 12px; padding-top: 10px;
               border-top: 1px solid rgba(255,255,255,0.15);
               display: flex; flex-direction: column; gap: 6px;",
      
      # Industry row
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        icon("industry", style = "font-size: 11px; opacity: 0.7; flex-shrink: 0;"),
        tags$span(style = "font-size: 12px; opacity: 0.85; flex: 1;",
                  industry_str),
        if (!is.null(industry_payload)) {
          tags$button(
            style = "background: rgba(255,255,255,0.15);
                     border: 1px solid rgba(255,255,255,0.3);
                     border-radius: 5px; padding: 2px 8px; color: white;
                     cursor: pointer; font-size: 11px; font-weight: 600;",
            onclick = sprintf(
              "Shiny.setInputValue('edit_industry_btn', %s,
               {priority: 'event'})",
              industry_payload),
            "Edit"
          )
        }
      ),
      
      # Reach row
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        icon("globe", style = "font-size: 11px; opacity: 0.7; flex-shrink: 0;"),
        tags$span(style = "font-size: 12px; opacity: 0.85; flex: 1;",
                  reach_str),
        if (!is.null(reach_payload)) {
          tags$button(
            style = "background: rgba(255,255,255,0.15);
                     border: 1px solid rgba(255,255,255,0.3);
                     border-radius: 5px; padding: 2px 8px; color: white;
                     cursor: pointer; font-size: 11px; font-weight: 600;",
            onclick = sprintf(
              "Shiny.setInputValue('edit_reach_btn', %s,
               {priority: 'event'})",
              reach_payload),
            "Edit"
          )
        }
      )
    )
  )
})

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
          <circle cx="50" cy="50" r="%d" fill="none"
                  stroke="#f0f0f0" stroke-width="8"/>
          <circle cx="50" cy="50" r="%d" fill="none"
                  stroke="%s" stroke-width="8"
                  stroke-dasharray="%.1f" stroke-dashoffset="%.1f"
                  stroke-linecap="round"
                  style="transition: stroke-dashoffset 0.8s ease;"/>
        </svg>',
                   radius, radius, text_color, circumference, dash_offset)),
      div(class = "gauge-text",
          style = paste0("color: ", text_color, ";"),
          paste0(used, "/", total))
    ),
    div(class = "gauge-label", label)
  )
}

output$account_brand_gauge <- renderUI({
  req(user_subscription(), user_competitor_count())
  sub  <- user_subscription()
  used <- user_competitor_count()
  max_comp <- sub$num_competitors_included + sub$extra_competitors_added
  render_gauge(used, max_comp, "#3498DB", "Competitor Slots")
})

output$account_query_gauge <- renderUI({
  req(user_subscription(), user_query_count())
  sub  <- user_subscription()
  used <- user_query_count()
  max_q <- sub$num_prompts_included + sub$extra_prompts_added
  render_gauge(used, max_q, "#667eea", "Prompt Slots")
})

output$account_persona_gauge <- renderUI({
  req(user_subscription(), user_persona_count())
  sub  <- user_subscription()
  used <- user_persona_count()
  max_personas <- (sub$num_personas_included %||% 0) +
    (sub$extra_personas_added  %||% 0)
  render_gauge(used, max_personas, "#8E44AD", "Persona Slots")
})

output$account_upgrade_card <- renderUI({
  sub          <- tryCatch(user_subscription(), error = function(e) NULL)
  current_plan <- if (!is.null(sub)) sub$subscription_name else "Free"
  
  if (current_plan == "Enterprise") {
    return(
      div(
        class = "account-card",
        style = "background: linear-gradient(135deg, #D4A843 0%, #B8922E 100%);
                 color: white; text-align: center;",
        icon("crown", class = "fa-2x",
             style = "margin-bottom: 8px; opacity: 0.9;"),
        div(style = "font-size: 15px; font-weight: 600; margin-bottom: 4px;",
            "Enterprise"),
        div(style = "font-size: 12px; opacity: 0.85;", "You have full access")
      )
    )
  }
  
  next_plan <- if (current_plan %in% c("Free", "Starter")) {
    list(
      name         = "Pro",
      monthly      = "$449/mo",
      annual       = "$349/mo",
      monthly_link = "https://buy.stripe.com/5kQ3cx3Kr8Cfci85zTgEg01",
      annual_link  = "https://buy.stripe.com/28E9AV6WD8Cfaa01jDgEg00",
      highlight    = "10 competitors, 10 prompts, 3 personas"
    )
  } else {
    list(
      name         = "Enterprise",
      monthly      = "Custom",
      annual       = "Custom",
      monthly_link = "https://airrscore.com/pricing",
      annual_link  = "https://airrscore.com/pricing",
      highlight    = "Unlimited everything + dedicated support"
    )
  }
  
  div(
    class = "account-card account-card-upgrade",
    icon("rocket", class = "fa-2x",
         style = "margin-bottom: 8px; opacity: 0.9;"),
    div(style = "font-size: 15px; font-weight: 600; margin-bottom: 2px;",
        paste0("Upgrade to ", next_plan$name)),
    div(style = "font-size: 12px; opacity: 0.85; margin-bottom: 4px;",
        next_plan$highlight),
    div(style = "font-size: 13px; font-weight: 700; margin-bottom: 12px;",
        next_plan$annual, " annual \u00b7 ", next_plan$monthly, " monthly"),
    div(
      style = "display: flex; gap: 6px; justify-content: center;",
      tags$a(
        href = next_plan$annual_link, target = "_blank",
        style = "background: rgba(255,255,255,0.25);
                 border: 2px solid rgba(255,255,255,0.6);
                 color: white; border-radius: 8px; font-weight: 600;
                 font-size: 12px; padding: 5px 12px; text-decoration: none;",
        "Annual"
      ),
      tags$a(
        href = next_plan$monthly_link, target = "_blank",
        style = "background: rgba(255,255,255,0.1);
                 border: 2px solid rgba(255,255,255,0.3);
                 color: white; border-radius: 8px; font-weight: 600;
                 font-size: 12px; padding: 5px 12px; text-decoration: none;",
        "Monthly"
      )
    )
  )
})

# ============================================
# Slot Badges
# ============================================

output$account_brand_slot_badge <- renderUI({
  req(user_subscription(), user_competitor_count())
  sub  <- user_subscription()
  used <- user_competitor_count()
  max_comp  <- sub$num_competitors_included + sub$extra_competitors_added
  remaining <- max_comp - used
  if (remaining > 0) {
    tags$span(class = "slot-badge available",
              paste0(remaining, " slot",
                     ifelse(remaining != 1, "s", ""), " remaining"))
  } else {
    tags$span(class = "slot-badge full", "No slots remaining")
  }
})

output$account_query_slot_badge <- renderUI({
  req(user_subscription(), user_query_count())
  sub  <- user_subscription()
  used <- user_query_count()
  max_q     <- sub$num_prompts_included + sub$extra_prompts_added
  remaining <- max_q - used
  if (remaining > 0) {
    tags$span(class = "slot-badge available",
              paste0(remaining, " slot",
                     ifelse(remaining != 1, "s", ""), " remaining"))
  } else {
    tags$span(class = "slot-badge full", "No slots remaining")
  }
})

output$account_persona_slot_badge <- renderUI({
  req(user_subscription(), user_persona_count())
  sub  <- user_subscription()
  used <- user_persona_count()
  max_personas <- (sub$num_personas_included %||% 0) +
    (sub$extra_personas_added  %||% 0)
  remaining <- max_personas - used
  if (remaining > 0) {
    tags$span(class = "slot-badge available",
              paste0(remaining, " slot",
                     ifelse(remaining != 1, "s", ""), " remaining"))
  } else {
    tags$span(class = "slot-badge full", "No slots remaining")
  }
})

# ============================================
# Low presence industry/reach nudge
# ============================================

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
  
  info <- tryCatch(
    dbGetQuery(pool, "
      SELECT b.brand_id, b.brand_name, b.brand_reach, b.reach_country,
             b.reach_region, b.reach_postcode, ubt.industry
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
  
  current_industry <- if (!is.null(info) && nrow(info) > 0) info$industry[1] else NA
  current_reach    <- if (!is.null(info) && nrow(info) > 0) {
    format_reach_display(info$brand_reach[1], info$reach_country[1],
                         info$reach_region[1], info$reach_postcode[1])
  } else "Global"
  
  industry_payload <- if (!is.null(info) && nrow(info) > 0) {
    jsonlite::toJSON(list(
      brand_id   = info$brand_id[1],
      brand_name = info$brand_name[1],
      industry   = info$industry[1] %||% ""
    ), auto_unbox = TRUE)
  } else NULL
  
  reach_payload <- if (!is.null(info) && nrow(info) > 0) {
    jsonlite::toJSON(list(
      brand_reach    = info$brand_reach[1]    %||% "global",
      reach_country  = info$reach_country[1]  %||% "",
      reach_region   = info$reach_region[1]   %||% "",
      reach_postcode = info$reach_postcode[1] %||% ""
    ), auto_unbox = TRUE)
  } else NULL
  
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
        div(style = "font-weight: 600; font-size: 14px; color: #C0392B; margin-bottom: 4px;",
            "Your Presence score is very low"),
        p(style = "font-size: 13px; color: #718096; margin: 0 0 10px; line-height: 1.5;",
          "This is usually caused by an industry that's too broad, a reach that's too wide,
           or both."),
        div(
          style = "display: flex; gap: 16px; margin-bottom: 12px; flex-wrap: wrap;",
          div(
            style = "font-size: 12px; color: #718096;",
            tags$span(style = "font-weight: 600;", "Industry: "),
            tags$span(
              style = "background: rgba(231,76,60,0.1); color: #C0392B;
                       padding: 2px 8px; border-radius: 4px; font-weight: 500;",
              if (!is.na(current_industry) && nzchar(current_industry %||% ""))
                current_industry else "Not set"
            )
          ),
          div(
            style = "font-size: 12px; color: #718096;",
            tags$span(style = "font-weight: 600;", "Reach: "),
            tags$span(
              style = "background: rgba(231,76,60,0.1); color: #C0392B;
                       padding: 2px 8px; border-radius: 4px; font-weight: 500;",
              current_reach
            )
          )
        ),
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
          if (!is.null(industry_payload)) {
            tags$button(
              style = "background: #E74C3C; color: white; border: none;
                       border-radius: 8px; padding: 7px 14px; font-weight: 600;
                       font-size: 12px; cursor: pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('edit_industry_btn', %s, {priority: 'event'})",
                industry_payload),
              icon("industry", style = "margin-right: 5px;"),
              "Update Industry"
            )
          },
          if (!is.null(reach_payload)) {
            tags$button(
              style = "background: #E74C3C; color: white; border: none;
                       border-radius: 8px; padding: 7px 14px; font-weight: 600;
                       font-size: 12px; cursor: pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('edit_reach_btn', %s, {priority: 'event'})",
                reach_payload),
              icon("globe", style = "margin-right: 5px;"),
              "Update Reach"
            )
          }
        )
      )
    )
  )
})

# ============================================
# Competitor List
# ============================================

output$account_competitor_list <- renderUI({
  req(rv$logged_in)
  rv$brands_refresh
  competitors <- user_competitors()
  
  if (nrow(competitors) == 0) {
    return(div(
      style = "text-align: center; padding: 30px;",
      icon("building", class = "fa-2x",
           style = "color: #e2e8f0; margin-bottom: 10px;"),
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
      } else "Active"
      
      meta_html <- tags$span(
        tags$span(style = "color: #667eea; font-weight: 600;", score_text),
        tags$span(style = "margin: 0 6px; color: #e2e8f0;", "\u00b7"),
        format(date_added, "%b %d, %Y")
      )
      icon_class <- "account-list-icon brand"
      icon_name  <- "building"
    } else {
      meta_html <- tags$span(
        tags$i(class = "fa fa-spinner fa-spin",
               style = "margin-right: 4px;"),
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
      div(
        class = "account-list-actions",
        tags$button(
          class   = "btn-remove",
          onclick = sprintf(
            "Shiny.setInputValue('remove_brand_id', %d, {priority: 'event'})",
            bid),
          icon("times")
        )
      )
    )
  })
  
  do.call(tagList, competitor_cards)
})

# ============================================
# Query List
# ============================================

output$account_query_list <- renderUI({
  req(rv$logged_in)
  rv$queries_refresh
  queries <- user_tracked_queries()
  
  if (nrow(queries) == 0) {
    return(div(
      style = "text-align: center; padding: 30px;",
      icon("comment-dots", class = "fa-2x",
           style = "color: #e2e8f0; margin-bottom: 10px;"),
      p(style = "color: #a0aec0; font-size: 13px; margin: 0;",
        "No prompts tracked yet")
    ))
  }
  
  query_cards <- lapply(1:nrow(queries), function(i) {
    qid        <- queries$query_id[i]
    qstring    <- queries$query_string[i]
    date_added <- queries$date_valid_from[i]
    
    div(
      class = "account-list-item",
      div(class = "account-list-icon query", icon("comment-dots")),
      div(class = "account-list-content",
          div(class = "name", qstring),
          div(class = "meta",
              paste("Added:", format(date_added, "%b %d, %Y")))),
      div(
        class = "account-list-actions",
        tags$button(
          class   = "btn-remove",
          onclick = sprintf(
            "Shiny.setInputValue('remove_query_id', %d, {priority: 'event'})",
            qid),
          icon("times")
        )
      )
    )
  })
  
  do.call(tagList, query_cards)
})

# ============================================
# Brand Aliases Section
# ============================================

output$account_alias_section <- renderUI({
  req(rv$logged_in, rv$login_id)
  alias_refresh()
  rv$brands_refresh
  
  # Get all active brands for this user (main + competitors)
  all_brands <- tryCatch(
    dbGetQuery(pool, "
      SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
      FROM fact_user_brands_tracked ubt
      JOIN dim_brand b ON b.brand_id = ubt.brand_id
      WHERE ubt.login_id = $1
        AND ubt.date_valid_from <= CURRENT_DATE
        AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
      ORDER BY ubt.main_brand_flag DESC, b.brand_name",
               params = list(rv$login_id)),
    error = function(e) NULL
  )
  
  if (is.null(all_brands) || nrow(all_brands) == 0) {
    return(div(
      style = "text-align: center; padding: 20px; color: #a0aec0;",
      p(style = "font-size: 13px; margin: 0;",
        "Add brands first to manage their aliases.")
    ))
  }
  
  div(
    lapply(1:nrow(all_brands), function(i) {
      bid     <- all_brands$brand_id[i]
      bname   <- all_brands$brand_name[i]
      is_main <- isTRUE(all_brands$main_brand_flag[i])
      aliases <- get_brand_aliases(bid)
      
      div(
        style = "margin-bottom: 10px; padding: 12px 14px;
                 background: #fafaf7; border-radius: 8px;
                 border: 1px solid #e2e8f0;",
        
        # Brand name header
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
          div(
            style = "width: 24px; height: 24px; border-radius: 6px;
                     background: rgba(102,126,234,0.1); display: flex;
                     align-items: center; justify-content: center;",
            icon(if (is_main) "building" else "building",
                 style = "font-size: 11px; color: #667eea;")
          ),
          tags$span(
            style = "font-weight: 600; font-size: 13px; color: #2d3748; flex: 1;",
            bname
          ),
          if (is_main) {
            tags$span(
              style = "background: #667eea; color: white; font-size: 9px;
                       padding: 2px 7px; border-radius: 8px; font-weight: 600;",
              "MAIN"
            )
          },
          if (nrow(aliases) > 0) {
            tags$span(
              style = "font-size: 11px; color: #a0aec0;",
              paste0(nrow(aliases), " alias",
                     if (nrow(aliases) != 1) "es" else "")
            )
          }
        ),
        
        # Existing aliases as chips
        if (nrow(aliases) > 0) {
          div(
            style = "display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 10px;",
            lapply(1:nrow(aliases), function(j) {
              div(
                style = "display: inline-flex; align-items: center; gap: 4px;
                         background: rgba(102,126,234,0.08);
                         border: 1px solid rgba(102,126,234,0.2);
                         border-radius: 12px; padding: 3px 10px; font-size: 12px;",
                tags$span(style = "color: #4a5568;", aliases$alias_name[j]),
                tags$button(
                  style = "background: none; border: none; color: #a0aec0;
                            cursor: pointer; font-size: 11px; padding: 0 2px;
                            line-height: 1;",
                  onclick = sprintf(
                    "Shiny.setInputValue('remove_alias_id', %d,
                     {priority: 'event'})",
                    aliases$alias_id[j]
                  ),
                  "\u00d7"
                )
              )
            })
          )
        },
        
        # Add alias row
        div(
          style = "display: flex; gap: 6px;",
          textInput(
            paste0("alias_input_", bid), NULL,
            placeholder = paste0("Add alias for \"", bname, "\"..."),
            width = "100%"
          ),
          tags$button(
            style = "background: #667eea; color: white; border: none;
                     border-radius: 6px; padding: 6px 14px; font-size: 12px;
                     font-weight: 600; cursor: pointer; white-space: nowrap;
                     height: 34px; margin-top: 1px;",
            onclick = sprintf(
              "var v = document.getElementById('alias_input_%d').value;
               if (v.trim().length > 0) {
                 Shiny.setInputValue('add_alias_brand_id', %d,
                   {priority: 'event'});
                 Shiny.setInputValue('add_alias_name', v.trim(),
                   {priority: 'event'});
               }",
              bid, bid
            ),
            "Add"
          )
        )
      )
    })
  )
})

# Add alias
observeEvent(input$add_alias_name, {
  req(rv$logged_in, rv$login_id)
  req(input$add_alias_brand_id, input$add_alias_name)
  
  alias <- trimws(input$add_alias_name)
  bid   <- as.integer(input$add_alias_brand_id)
  
  if (nchar(alias) < 2) return()
  
  success <- add_brand_alias(bid, alias)
  if (success) {
    showNotification(
      paste0("\u2713 Alias \"", alias, "\" added"),
      type = "message", duration = 3)
    alias_refresh(alias_refresh() + 1)
    updateTextInput(session, paste0("alias_input_", bid), value = "")
  } else {
    showNotification("Error adding alias — it may already exist.",
                     type = "error", duration = 3)
  }
})

# Remove alias
observeEvent(input$remove_alias_id, {
  req(rv$logged_in, rv$login_id)
  
  success <- remove_brand_alias(as.integer(input$remove_alias_id))
  if (success) {
    showNotification("\u2713 Alias removed", type = "message", duration = 3)
    alias_refresh(alias_refresh() + 1)
  } else {
    showNotification("Error removing alias.", type = "error", duration = 3)
  }
})

# ============================================
# Add Competitor Handler
# ============================================

observeEvent(input$add_competitor_btn, {
  req(rv$logged_in, rv$login_id)
  
  if (isTRUE(rv$is_demo)) {
    showNotification("Demo mode — sign up to add competitors.",
                     type = "warning", duration = 4)
    return()
  }
  
  brand_name <- trimws(input$add_competitor_brand_input)
  
  if (brand_name == "") {
    showNotification("Please enter a brand name.", type = "error", duration = 3)
    return()
  }
  
  main <- user_main_brand()
  if (nrow(main) > 0 &&
      tolower(brand_name) == tolower(main$brand_name)) {
    showNotification("This is already your main brand!",
                     type = "warning", duration = 3)
    return()
  }
  
  existing <- user_competitors()
  if (nrow(existing) > 0 &&
      tolower(brand_name) %in% tolower(existing$brand_name)) {
    showNotification("You're already tracking this brand.",
                     type = "warning", duration = 3)
    return()
  }
  
  sub  <- user_subscription()
  used <- user_competitor_count()
  max_competitors <- sub$num_competitors_included + sub$extra_competitors_added
  
  if (used >= max_competitors) {
    showNotification(
      paste0("You've used all ", max_competitors,
             " competitor slots. Upgrade to add more!"),
      type = "error", duration = 5)
    return()
  }
  
  user_industry <- dbGetQuery(pool, "
    SELECT ubt.industry
    FROM fact_user_brands_tracked ubt
    WHERE ubt.login_id = $1
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LIMIT 1",
                              params = list(rv$login_id))
  
  industry <- if (nrow(user_industry) > 0 &&
                  !is.na(user_industry$industry[1])) {
    user_industry$industry[1]
  } else NULL
  
  brand_result <- add_brand_for_user_pending(
    rv$login_id, brand_name, main_brand = FALSE, industry = industry)
  
  if (is.null(brand_result)) {
    showNotification("Error adding brand. Please try again.",
                     type = "error", duration = 3)
    return()
  }
  
  link_existing_queries_to_brand(rv$login_id, brand_result$brand_id)
  
  updateTextInput(session, "add_competitor_brand_input", value = "")
  rv$brands_refresh  <- rv$brands_refresh + 1
  rv$queries_refresh <- rv$queries_refresh + 1
  
  user_queries <- get_user_tracked_queries(rv$login_id)
  prompt_map <- if (nrow(user_queries) > 0) {
    setNames(as.list(user_queries$query_id), user_queries$query_string)
  } else list()
  
  dbExecute(pool, "
    INSERT INTO dim_job_queue (job_type, login_id, payload)
    VALUES ('score_competitor', $1, $2)",
            params = list(rv$login_id, toJSON(list(
              brand_name = brand_name,
              brand_id   = brand_result$brand_id,
              prompts    = prompt_map
            ), auto_unbox = TRUE)))
  
  showNotification(
    paste0("\u2713 ", brand_name,
           " added! Scores calculating in background."),
    type = "message", duration = 5)
  
  NULL
})

# ============================================
# Add Query Handler
# ============================================

observeEvent(input$add_query_btn, {
  req(rv$logged_in, rv$login_id)
  
  if (isTRUE(rv$is_demo)) {
    showNotification("Demo mode — sign up to add prompts.",
                     type = "warning", duration = 4)
    return()
  }
  
  query_string <- trimws(input$add_query_input)
  
  if (query_string == "") {
    showNotification("Please enter a prompt.", type = "error", duration = 3)
    return()
  }
  
  existing <- user_tracked_queries()
  if (nrow(existing) > 0 &&
      tolower(query_string) %in% tolower(existing$query_string)) {
    showNotification("You're already tracking this prompt.",
                     type = "warning", duration = 3)
    return()
  }
  
  sub  <- user_subscription()
  used <- user_query_count()
  max_queries <- sub$num_prompts_included + sub$extra_prompts_added
  
  if (used >= max_queries) {
    showNotification(
      paste0("You've used all ", max_queries,
             " prompt slots. Upgrade to add more!"),
      type = "error", duration = 5)
    return()
  }
  
  query_result <- add_query_for_user(rv$login_id, query_string)
  
  if (is.null(query_result)) {
    showNotification("Error adding prompt. Please try again.",
                     type = "error", duration = 3)
    return()
  }
  
  updateTextInput(session, "add_query_input", value = "")
  rv$queries_refresh <- rv$queries_refresh + 1
  rv$brands_refresh  <- rv$brands_refresh  + 1
  
  user_brands <- get_user_brands(rv$login_id)
  brand_ids   <- user_brands$brand_id
  
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
  
  if (isTRUE(rv$is_demo)) return()
  
  brand_id  <- input$remove_brand_id
  brand_info <- dbGetQuery(pool,
                           "SELECT brand_name FROM dim_brand WHERE brand_id = $1",
                           params = list(brand_id))
  
  success <- remove_brand_for_user(rv$login_id, brand_id)
  if (success) {
    showNotification(
      paste0("\u2713 ", brand_info$brand_name, " removed"),
      type = "message", duration = 3)
    rv$brands_refresh  <- rv$brands_refresh  + 1
    rv$queries_refresh <- rv$queries_refresh + 1
  } else {
    showNotification("Error removing brand", type = "error", duration = 3)
  }
})

observeEvent(input$remove_query_id, {
  req(rv$logged_in, rv$login_id)
  
  if (isTRUE(rv$is_demo)) return()
  
  query_id <- input$remove_query_id
  
  success <- remove_query_for_user(rv$login_id, query_id)
  if (success) {
    showNotification("\u2713 Prompt removed", type = "message", duration = 3)
    rv$queries_refresh <- rv$queries_refresh + 1
    rv$brands_refresh  <- rv$brands_refresh  + 1
  } else {
    showNotification("Error removing prompt", type = "error", duration = 3)
  }
})

# ============================================
# Timing notice outputs
# ============================================

output$account_competitor_timing_notice <- renderUI({
  req(rv$logged_in, rv$login_id)
  brand_input <- input$add_competitor_brand_input
  if (is.null(brand_input) || nchar(trimws(brand_input)) < 2) return(NULL)
  est <- tryCatch(estimate_competitor_add_time(rv$login_id), error = function(e) NULL)
  if (is.null(est)) return(NULL)
  render_timing_notice(est$time_str, est$breakdown, "competitor")
})

output$account_prompt_timing_notice <- renderUI({
  req(rv$logged_in, rv$login_id)
  prompt_input <- input$add_query_input
  if (is.null(prompt_input) || nchar(trimws(prompt_input)) < 2) return(NULL)
  est <- tryCatch(estimate_prompt_add_time(rv$login_id), error = function(e) NULL)
  if (is.null(est)) return(NULL)
  render_timing_notice(est$time_str, est$breakdown, "prompt")
})

# ============================================
# Upgrade Modal
# ============================================

observeEvent(input$upgrade_btn, {
  showModal(modalDialog(
    title = div(style = "font-weight: 600;", "Upgrade Your Plan"),
    size  = "l",
    
    div(
      style = "display: flex; gap: 15px;",
      
      # Starter
      div(
        style = "flex: 1; text-align: center; padding: 25px 15px;
                 border: 2px solid #3498DB; border-radius: 12px;",
        div(style = "color: #3498DB;", icon("seedling", class = "fa-2x")),
        h4(style = "margin: 10px 0 0;", "Starter"),
        h3(style = "color: #3498DB; margin: 5px 0;", "$99"),
        p(style = "color: #a0aec0; font-size: 12px;",
          "per month, billed annually"),
        p(style = "color: #a0aec0; font-size: 11px;", "or $129/mo monthly"),
        hr(style = "border-color: #f0f0f0;"),
        div(
          style = "font-size: 13px; text-align: left; padding: 0 10px;",
          p("\u2713 5 competitors"),
          p("\u2713 1 prompt"),
          p("\u2713 1 persona"),
          p("\u2713 30-day history")
        ),
        br(),
        div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          tags$a(href = "https://buy.stripe.com/00wbJ36WD2dRci86DXgEg03",
                 target = "_blank", class = "btn btn-primary",
                 style = "border-radius: 8px; background: #3498DB;
                          border-color: #3498DB;",
                 "Annual \u2014 $99/mo"),
          tags$a(href = "https://buy.stripe.com/dRmeVfep5cSv0zq9Q9gEg04",
                 target = "_blank", class = "btn btn-default",
                 style = "border-radius: 8px;",
                 "Monthly \u2014 $129/mo")
        )
      ),
      
      # Pro
      div(
        style = "flex: 1; text-align: center; padding: 25px 15px;
                 border: 2px solid #667eea; border-radius: 12px;
                 box-shadow: 0 4px 15px rgba(102,126,234,0.15);
                 position: relative;",
        tags$span(
          style = "position: absolute; top: -12px; left: 50%;
                   transform: translateX(-50%);
                   background: #667eea; color: white; font-size: 11px;
                   font-weight: 600; padding: 3px 12px; border-radius: 20px;",
          "POPULAR"
        ),
        div(style = "color: #667eea;", icon("gem", class = "fa-2x")),
        h4(style = "margin: 10px 0 0;", "Pro"),
        h3(style = "color: #667eea; margin: 5px 0;", "$349"),
        p(style = "color: #a0aec0; font-size: 12px;",
          "per month, billed annually"),
        p(style = "color: #a0aec0; font-size: 11px;", "or $449/mo monthly"),
        hr(style = "border-color: #f0f0f0;"),
        div(
          style = "font-size: 13px; text-align: left; padding: 0 10px;",
          p(strong("\u2713 10 competitors")),
          p(strong("\u2713 10 prompts")),
          p(strong("\u2713 3 personas")),
          p("\u2713 90-day history"),
          p("\u2713 CSV & API export")
        ),
        br(),
        div(
          style = "display: flex; flex-direction: column; gap: 6px;",
          tags$a(href = "https://buy.stripe.com/28E9AV6WD8Cfaa01jDgEg00",
                 target = "_blank", class = "btn btn-primary",
                 style = "border-radius: 8px; background: #667eea;
                          border-color: #667eea;",
                 "Annual \u2014 $349/mo"),
          tags$a(href = "https://buy.stripe.com/5kQ3cx3Kr8Cfci85zTgEg01",
                 target = "_blank", class = "btn btn-default",
                 style = "border-radius: 8px;",
                 "Monthly \u2014 $449/mo")
        )
      ),
      
      # Enterprise
      div(
        style = "flex: 1; text-align: center; padding: 25px 15px;
                 border: 2px solid #D4A843; border-radius: 12px;",
        div(style = "color: #D4A843;", icon("crown", class = "fa-2x")),
        h4(style = "margin: 10px 0 0;", "Enterprise"),
        h3(style = "color: #D4A843; margin: 5px 0;", "Custom"),
        p(style = "color: #a0aec0; font-size: 12px;", "tailored pricing"),
        hr(style = "border-color: #f0f0f0;"),
        div(
          style = "font-size: 13px; text-align: left; padding: 0 10px;",
          p(strong("\u2713 Unlimited competitors")),
          p(strong("\u2713 Unlimited prompts")),
          p(strong("\u2713 Unlimited personas")),
          p("\u2713 Multi-LLM coverage"),
          p("\u2713 Dedicated support"),
          p("\u2713 Custom integrations")
        ),
        br(),
        tags$a(href = "https://airrscore.com/pricing", target = "_blank",
               class = "btn btn-warning",
               style = "width: 100%; border-radius: 8px;",
               "Contact Sales")
      )
    ),
    
    div(
      style = "text-align: center; margin-top: 20px; padding: 12px;
               background: rgba(212,168,67,0.06); border-radius: 8px;
               border: 1px solid rgba(212,168,67,0.2);",
      icon("star", style = "color: #D4A843; margin-right: 6px;"),
      tags$span(
        style = "font-size: 12px; color: #718096;",
        tags$strong(style = "color: #D4A843;", "Founding Member Rates \u2014 "),
        "prices shown are locked in for early customers. Cancel anytime."
      )
    ),
    
    footer = modalButton("Close"),
    easyClose = TRUE
  ))
})

observeEvent(input$upgrade_from_api, {
  shinyjs::click("upgrade_btn")
})