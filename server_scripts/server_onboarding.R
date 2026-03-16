# ============================================
# Onboarding Flow
# ============================================

onboarding_step <- reactiveVal(1)

onboarding_data <- reactiveValues(
  brand_name     = NULL,
  brand_reach    = NULL,
  reach_country  = NULL,
  reach_region   = NULL,
  reach_postcode = NULL,
  industry       = NULL,
  competitors    = NULL,
  prompts        = NULL
)

setup_time_estimate <- reactive({
  n_competitors <- length(selected_competitors())
  n_prompts     <- length(selected_prompts())
  n_personas    <- length(ob_selected_personas())
  estimate_setup_time(n_competitors, n_prompts, n_personas)
})

output$setup_time_estimate_ui <- renderUI({
  est <- setup_time_estimate()
  
  n_competitors <- length(selected_competitors())
  n_prompts     <- length(selected_prompts())
  n_personas    <- length(ob_selected_personas())
  n_brands      <- n_competitors + 1
  
  div(
    style = "background: rgba(102,126,234,0.06); border: 1px solid rgba(102,126,234,0.2);
             border-radius: 10px; padding: 14px 16px; margin-top: 16px;",
    
    # Header
    div(
      style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
      icon("clock", style = "color: #667eea; font-size: 14px;"),
      tags$span(
        style = "font-weight: 600; font-size: 13px; color: #2d3748;",
        paste0("Your dashboard will be ready in ", est$time_str)
      )
    ),
    
    # Breakdown
    div(
      style = "display: flex; flex-direction: column; gap: 4px;",
      
      div(
        style = "display: flex; justify-content: space-between; 
                 font-size: 12px; color: #718096;",
        tags$span(paste0(n_brands, " brand", if (n_brands != 1) "s" else "", " to score")),
        tags$span(paste0("~", round(est$brand_seconds / 60), " min"))
      ),
      
      if (n_prompts > 0) {
        div(
          style = "display: flex; justify-content: space-between;
                   font-size: 12px; color: #718096;",
          tags$span(paste0(n_prompts, " prompt", if (n_prompts != 1) "s" else "",
                           " × ", n_brands, " brand", if (n_brands != 1) "s" else "")),
          tags$span(paste0("~", round(est$prompt_seconds / 60), " min"))
        )
      },
      
      if (n_personas > 0) {
        div(
          style = "display: flex; justify-content: space-between;
                   font-size: 12px; color: #718096;",
          tags$span(paste0(n_personas, " persona", if (n_personas != 1) "s" else "",
                           " × ", n_brands, " brand", if (n_brands != 1) "s" else "")),
          tags$span(paste0("~", round(est$persona_seconds / 60), " min"))
        )
      },
      
      # Divider
      div(style = "height: 1px; background: rgba(102,126,234,0.2); margin: 6px 0;"),
      
      div(
        style = "display: flex; justify-content: space-between;
                 font-size: 13px; font-weight: 600; color: #667eea;",
        tags$span("Total estimated wait"),
        tags$span(est$time_str)
      )
    ),
    
    # Reassurance message
    div(
      style = "margin-top: 10px; font-size: 11px; color: #a0aec0; line-height: 1.5;",
      icon("info-circle", style = "margin-right: 4px;"),
      "Scores calculate in the background, you'll see spinners on your dashboard 
       and results will appear automatically as they complete. 
       You don't need to keep this page open."
    )
  )
})

# ============================================
# Progress Bar — 6 steps
# ============================================

output$onboarding_progress_bar <- renderUI({
  step        <- onboarding_step()
  total_steps <- 6
  pct         <- round((step - 1) / total_steps * 100)
  step_labels <- c("Brand", "Reach", "Industry", "Competitors", "Prompts", "Personas")
  
  div(
    div(
      style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
      lapply(seq_along(step_labels), function(i) {
        is_done    <- i < step
        is_current <- i == step
        color <- if (is_done) "#27AE60" else if (is_current) "#667eea" else "#cbd5e0"
        bg    <- if (is_done || is_current) color else "transparent"
        
        div(
          style = "display: flex; flex-direction: column; align-items: center; flex: 1;",
          div(
            style = paste0(
              "width: 32px; height: 32px; border-radius: 50%; border: 2px solid ", color, "; ",
              "background: ", bg, "; color: ", if (is_done || is_current) "white" else color, "; ",
              "display: flex; align-items: center; justify-content: center; ",
              "font-weight: 700; font-size: 13px; margin: 0 auto 4px;"
            ),
            if (is_done) icon("check") else i
          ),
          tags$span(
            style = paste0(
              "font-size: 10px; font-weight: ", if (is_current) "600" else "400", "; ",
              "color: ", color, "; text-transform: uppercase; letter-spacing: 0.5px;"
            ),
            step_labels[i]
          )
        )
      })
    ),
    div(
      style = "height: 4px; background: #f0f0f0; border-radius: 2px; margin-bottom: 24px;",
      div(style = paste0(
        "height: 100%; border-radius: 2px; background: #667eea; ",
        "width: ", pct, "%; transition: width 0.4s ease;"
      ))
    )
  )
})

# ============================================
# Step UI Router — 6 steps
# ============================================

output$onboarding_step_ui <- renderUI({
  switch(as.character(onboarding_step()),
         "1" = onboarding_step1_ui(),
         "2" = onboarding_step2_ui(),
         "3" = onboarding_step3_ui(),
         "4" = onboarding_step4_ui(),
         "5" = onboarding_step5_ui(),
         "6" = onboarding_step6_ui()
  )
})

# ============================================
# Step 1: Brand Name
# ============================================

onboarding_step1_ui <- function() {
  div(
    class = "login-container",
    style = "max-width: 620px;",
    div(
      class = "login-header",
      h2("What's your brand?"),
      p("Tell us the name of the brand you want to track.")
    ),
    div(
      class = "login-body",
      uiOutput("step1_alert"),
      div(
        style = "margin-bottom: 20px;",
        tags$label(
          style = "font-weight: 600; font-size: 13px; color: #4a5568;
                   text-transform: uppercase; letter-spacing: 0.5px;
                   margin-bottom: 8px; display: block;",
          "Brand Name"
        ),
        textInput(
          "ob_brand_name", NULL,
          placeholder = "e.g. Nike, Apple, Acme Corp...",
          width = "100%"
        )
      ),
      actionButton(
        "ob_step1_next", "Continue",
        style = "width: 100%; height: 44px; background: #1A1A1A; color: #D4A843;
                 border: 2px solid #D4A843; border-radius: 8px; font-weight: 600;
                 font-size: 15px;"
      )
    )
  )
}

output$step1_alert <- renderUI({ NULL })

observeEvent(input$ob_step1_next, {
  brand <- trimws(input$ob_brand_name)
  if (nchar(brand) < 2) {
    output$step1_alert <- renderUI({
      div(class = "alert alert-danger",
          "Please enter your brand name (at least 2 characters).")
    })
    return()
  }
  onboarding_data$brand_name <- brand
  onboarding_step(2)
})

# ============================================
# Step 2: Brand Reach
# ============================================

selected_reach <- reactiveVal(NULL)

onboarding_step2_ui <- function() {
  div(
    class = "login-container",
    style = "max-width: 620px;",
    div(
      class = "login-header",
      h2("What's your brand's reach?"),
      p("This helps us tailor your AI queries to the right market.")
    ),
    div(
      class = "login-body",
      uiOutput("step2_alert"),
      uiOutput("reach_cards_ui"),
      
      conditionalPanel(
        condition = "input.ob_brand_reach == 'national'",
        div(
          style = "margin-bottom: 16px;",
          tags$label(
            style = "font-weight: 600; font-size: 13px; color: #4a5568;
                     margin-bottom: 8px; display: block;",
            "Select your country"
          ),
          selectInput(
            "ob_country", NULL,
            choices = c("Select a country..." = "", get_country_list()),
            width = "100%"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.ob_brand_reach == 'regional'",
        div(
          style = "margin-bottom: 16px;",
          tags$label(
            style = "font-weight: 600; font-size: 13px; color: #4a5568;
                     margin-bottom: 8px; display: block;",
            "Define your region"
          ),
          p(
            style = "font-size: 12px; color: #a0aec0; margin-bottom: 8px;",
            "Be specific, e.g. 'The State of New York', 'Houston Texas', ",
            "'East Coast USA', 'South West England', 'Greater London'"
          ),
          textInput(
            "ob_region", NULL,
            placeholder = "e.g. The State of New York",
            width = "100%"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.ob_brand_reach == 'near_me'",
        div(
          style = "margin-bottom: 16px;",
          tags$label(
            style = "font-weight: 600; font-size: 13px; color: #4a5568;
                     margin-bottom: 8px; display: block;",
            "Your location"
          ),
          p(
            style = "font-size: 12px; color: #a0aec0; margin-bottom: 8px;",
            "We'll use this to find what's relevant near you."
          ),
          fluidRow(
            column(
              width = 6,
              selectInput(
                "ob_nearme_country", "Country",
                choices = c("Select a country..." = "", get_country_list()),
                width = "100%"
              )
            ),
            column(
              width = 6,
              textInput(
                "ob_nearme_postcode", "Zip / Postcode",
                placeholder = "e.g. 10001 or SW1A 1AA",
                width = "100%"
              )
            )
          )
        )
      ),
      
      div(
        style = "display: flex; gap: 10px; margin-top: 8px;",
        actionButton(
          "ob_step2_back", "Back",
          style = "flex: 1; height: 44px; background: transparent; color: #9E9E9E;
                   border: 2px solid #e2e8f0; border-radius: 8px; font-weight: 600;"
        ),
        actionButton(
          "ob_step2_next", "Continue",
          style = "flex: 2; height: 44px; background: #1A1A1A; color: #D4A843;
                   border: 2px solid #D4A843; border-radius: 8px; font-weight: 600;
                   font-size: 15px;"
        )
      )
    )
  )
}

output$reach_cards_ui <- renderUI({
  current_reach <- selected_reach()
  
  reach_options <- list(
    list(val = "global",   icon_name = "globe",               label = "Global",
         desc = "Operates worldwide"),
    list(val = "national", icon_name = "flag",                label = "National",
         desc = "Single country"),
    list(val = "regional", icon_name = "map-marker-alt",      label = "Regional",
         desc = "Specific region or city"),
    list(val = "near_me",  icon_name = "location-crosshairs", label = "Near Me",
         desc = "Local / postcode-based")
  )
  
  div(
    style = "display: flex; gap: 12px; margin-bottom: 20px;",
    lapply(reach_options, function(opt) {
      is_selected <- !is.null(current_reach) && current_reach == opt$val
      
      div(
        style = "flex: 1;",
        tags$input(
          type  = "radio", name = "ob_reach_radio",
          id    = paste0("reach_", opt$val), value = opt$val,
          style = "display: none;",
          checked = if (is_selected) "checked" else NULL
        ),
        div(
          style = paste0(
            "display: block; border-radius: 12px; padding: 16px 12px; ",
            "text-align: center; cursor: pointer; transition: all 0.2s ease; ",
            if (is_selected) {
              paste0(
                "border: 2px solid #667eea; ",
                "background: linear-gradient(135deg, rgba(102,126,234,0.08) 0%, ",
                "rgba(118,75,162,0.08) 100%); ",
                "box-shadow: 0 0 0 3px rgba(102,126,234,0.15);"
              )
            } else {
              "border: 2px solid #e2e8f0; background: white;"
            }
          ),
          onclick = sprintf(
            "Shiny.setInputValue('ob_brand_reach', '%s', {priority: 'event'})",
            opt$val
          ),
          div(
            style = paste0(
              "font-size: 24px; margin-bottom: 10px; ",
              if (is_selected) "color: #667eea;" else "color: #a0aec0;"
            ),
            icon(opt$icon_name)
          ),
          div(
            style = paste0(
              "font-weight: 700; font-size: 14px; margin-bottom: 4px; ",
              if (is_selected) "color: #667eea;" else "color: #2d3748;"
            ),
            opt$label
          ),
          div(
            style = paste0(
              "font-size: 12px; ",
              if (is_selected) "color: #667eea; opacity: 0.8;" else "color: #a0aec0;"
            ),
            opt$desc
          ),
          if (is_selected) {
            div(
              style = "margin-top: 8px;",
              tags$span(
                style = "background: #667eea; color: white; font-size: 11px;
                         padding: 2px 10px; border-radius: 10px; font-weight: 600;",
                icon("check", style = "margin-right: 4px; font-size: 10px;"),
                "Selected"
              )
            )
          }
        )
      )
    })
  )
})

observeEvent(input$ob_brand_reach, { selected_reach(input$ob_brand_reach) })

output$step2_alert <- renderUI({ NULL })

observeEvent(input$ob_step2_back, { selected_reach(NULL); onboarding_step(1) })

observeEvent(input$ob_step2_next, {
  reach <- input$ob_brand_reach
  
  if (is.null(reach) || reach == "") {
    output$step2_alert <- renderUI({
      div(class = "alert alert-danger", "Please select your brand's reach.")
    })
    return()
  }
  
  if (reach == "national") {
    country <- input$ob_country
    if (is.null(country) || country == "") {
      output$step2_alert <- renderUI({
        div(class = "alert alert-danger", "Please select your country.")
      })
      return()
    }
    onboarding_data$reach_country  <- country
    onboarding_data$reach_region   <- NULL
    onboarding_data$reach_postcode <- NULL
  }
  
  if (reach == "regional") {
    region <- trimws(input$ob_region)
    if (nchar(region) < 3) {
      output$step2_alert <- renderUI({
        div(class = "alert alert-danger",
            "Please define your region (e.g. 'The State of New York').")
      })
      return()
    }
    onboarding_data$reach_region   <- region
    onboarding_data$reach_country  <- NULL
    onboarding_data$reach_postcode <- NULL
  }
  
  if (reach == "near_me") {
    nm_country  <- input$ob_nearme_country
    nm_postcode <- trimws(input$ob_nearme_postcode)
    if (is.null(nm_country) || nm_country == "") {
      output$step2_alert <- renderUI({
        div(class = "alert alert-danger", "Please select your country.")
      })
      return()
    }
    if (nchar(nm_postcode) < 2) {
      output$step2_alert <- renderUI({
        div(class = "alert alert-danger", "Please enter your zip / postcode.")
      })
      return()
    }
    onboarding_data$reach_country  <- nm_country
    onboarding_data$reach_postcode <- nm_postcode
    onboarding_data$reach_region   <- NULL
  }
  
  if (reach == "global") {
    onboarding_data$reach_country  <- NULL
    onboarding_data$reach_region   <- NULL
    onboarding_data$reach_postcode <- NULL
  }
  
  onboarding_data$brand_reach <- reach
  industry_lookup_trigger(industry_lookup_trigger() + 1)
  onboarding_step(3)
})

# ============================================
# Step 3: Industry
# ============================================

industry_lookup_trigger <- reactiveVal(0)
industry_loading        <- reactiveVal(FALSE)

onboarding_step3_ui <- function() {
  div(
    class = "login-container",
    style = "max-width: 620px;",
    div(
      class = "login-header",
      h2("What industry are you in?"),
      p("We've looked this up based on your brand, feel free to edit it.")
    ),
    div(
      class = "login-body",
      uiOutput("step3_alert"),
      
      # Guidance box
      div(
        style = "background: rgba(102,126,234,0.06);
                 border: 1px solid rgba(102,126,234,0.2);
                 border-radius: 10px; padding: 14px 16px; margin-bottom: 20px;",
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
          icon("lightbulb", style = "color: #667eea; font-size: 14px;"),
          tags$span(
            style = "font-weight: 600; font-size: 13px; color: #2d3748;",
            "Choose the industry that best describes you ", tags$em("and"), " your direct competitors"
          )
        ),
        p(
          style = "font-size: 12px; color: #718096; margin-bottom: 10px; line-height: 1.5;",
          "Be as specific as possible, a niche description gives much more meaningful scores
           than a broad category. Examples:"
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 5px;",
          lapply(list(
            list(brand = "Stripe",      broad = "Financial Services", 
                 niche = "Online Payment Infrastructure"),
            list(brand = "Databricks",  broad = "Software",           
                 niche = "Lakehouse Data Platform"),
            list(brand = "Peloton",     broad = "Fitness",            
                 niche = "Connected Fitness Hardware"),
            list(brand = "Rivian",      broad = "Automotive",         
                 niche = "Electric Adventure Vehicles"),
            list(brand = "Oatly",       broad = "Food & Beverage",    
                 niche = "Oat-based Dairy Alternatives")
          ), function(ex) {
            div(
              style = "display: flex; align-items: baseline; gap: 6px;
                       font-size: 12px; line-height: 1.6;",
              tags$span(
                style = "flex: 0 0 80px; font-weight: 600; color: #4a5568;",
                ex$brand
              ),
              tags$span(
                style = "color: #E74C3C; text-decoration: line-through; 
                         flex: 0 0 130px;",
                ex$broad
              ),
              icon("arrow-right", 
                   style = "font-size: 10px; color: #a0aec0; flex-shrink: 0;"),
              tags$span(
                style = "color: #27AE60; font-weight: 600;",
                ex$niche
              )
            )
          })
        )
      ),
      
      uiOutput("industry_field_ui"),
      
      div(
        style = "display: flex; gap: 10px; margin-top: 8px;",
        actionButton(
          "ob_step3_back", "Back",
          style = "flex: 1; height: 44px; background: transparent; color: #9E9E9E;
                   border: 2px solid #e2e8f0; border-radius: 8px; font-weight: 600;"
        ),
        actionButton(
          "ob_step3_next", "Continue",
          style = "flex: 2; height: 44px; background: #1A1A1A; color: #D4A843;
                   border: 2px solid #D4A843; border-radius: 8px; font-weight: 600;
                   font-size: 15px;"
        )
      )
    )
  )
}

output$step3_alert <- renderUI({ NULL })

observeEvent(industry_lookup_trigger(), {
  req(industry_lookup_trigger() > 0)
  req(onboarding_data$brand_name)
  
  industry_loading(TRUE)
  
  brand         <- onboarding_data$brand_name
  reach         <- onboarding_data$brand_reach
  country       <- onboarding_data$reach_country
  region        <- onboarding_data$reach_region
  reach_context <- build_reach_context(reach, country, region)
  
  future_promise({
    reach_str <- if (nzchar(reach_context)) paste0(" (", reach_context, ")") else ""
    
    prompt_text <- paste0(
      'What industry is the brand "', brand, '"', reach_str, ' in? ',
      'Reply with ONLY the industry name, nothing else. ',
      'Example replies: "Athletic Footwear", "Fast Food", "Investment Banking", ',
      '"Cloud Computing". Do not include the word "industry" in your reply.'
    )
    
    results <- future.apply::future_lapply(
      seq_len(10),
      function(i) {
        tryCatch({
          resp <- request("https://api.openai.com/v1/chat/completions") |>
            req_headers(
              Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
              `Content-Type` = "application/json"
            ) |>
            req_body_json(list(
              model       = "gpt-4.1-mini",
              messages    = list(list(role = "user", content = prompt_text)),
              temperature = 0.2,
              max_tokens  = 15
            )) |>
            req_perform()
          
          parsed <- resp_body_json(resp)
          raw    <- trimws(parsed$choices[[1]]$message$content)
          raw    <- gsub('^["\']|["\']$', '', raw)
          raw    <- gsub('[.!?,;]+$', '', raw)
          raw    <- gsub('\\s*industry\\s*$', '', raw, ignore.case = TRUE)
          trimws(raw)
        }, error = function(e) NA_character_)
      },
      future.seed = TRUE
    )
    
    industry_vec <- unlist(results)
    industry_vec <- industry_vec[!is.na(industry_vec) & nchar(industry_vec) > 0]
    if (length(industry_vec) == 0) return("")
    
    tab          <- sort(table(tolower(industry_vec)), decreasing = TRUE)
    winner_lower <- names(tab)[1]
    industry_vec[tolower(industry_vec) == winner_lower][1]
    
  }, seed = TRUE) %...>% (function(industry) {
    onboarding_data$industry <- industry
    industry_loading(FALSE)
  }) %...!% (function(err) {
    onboarding_data$industry <- ""
    industry_loading(FALSE)
  })
})

output$industry_field_ui <- renderUI({
  if (industry_loading()) {
    return(div(
      style = "text-align: center; padding: 20px;",
      tags$i(class = "fa fa-spinner fa-spin",
             style = "color: #667eea; font-size: 20px;"),
      p(style = "color: #a0aec0; margin-top: 8px; font-size: 13px;",
        "Looking up your industry...")
    ))
  }
  
  # Never check presence scores during onboarding — user has none yet
  tagList(
    div(
      tags$label(
        style = "font-weight: 600; font-size: 13px; color: #4a5568;
                 text-transform: uppercase; letter-spacing: 0.5px;
                 margin-bottom: 8px; display: block;",
        "Industry"
      ),
      textInput(
        "ob_industry", NULL,
        value       = onboarding_data$industry %||% "",
        placeholder = "e.g. Connected Fitness Hardware",
        width       = "100%"
      ),
      p(style = "font-size: 12px; color: #a0aec0; margin-top: 4px;",
        "Edit this if it doesn't look right.")
    )
  )
})

observeEvent(input$ob_step3_back, { onboarding_step(2) })

observeEvent(input$ob_step3_next, {
  
  # Guard against industry field not yet rendered
  industry <- trimws(input$ob_industry %||% "")
  
  if (nchar(industry) < 2) {
    output$step3_alert <- renderUI({
      div(class = "alert alert-danger", "Please enter your industry.")
    })
    return()
  }
  
  onboarding_data$industry <- industry
  competitor_lookup_trigger(competitor_lookup_trigger() + 1)
  onboarding_step(4)
})

# ============================================
# Step 4: Competitors
# ============================================

competitor_lookup_trigger <- reactiveVal(0)
competitor_loading        <- reactiveVal(FALSE)
suggested_competitors     <- reactiveVal(character(0))
selected_competitors      <- reactiveVal(character(0))

onboarding_step4_ui <- function() {
  div(
    class = "login-container",
    style = "max-width: 620px;",
    div(
      class = "login-header",
      h2("Who are your competitors?"),
      p("We've suggested the top competitors, remove any that aren't relevant, or add your own.")
    ),
    div(
      class = "login-body",
      uiOutput("step4_alert"),
      uiOutput("competitor_selection_ui"),
      
      div(
        style = "margin-top: 16px;",
        tags$label(
          style = "font-weight: 600; font-size: 13px; color: #4a5568;
                   margin-bottom: 8px; display: block;",
          "Add a competitor manually"
        ),
        div(
          style = "display: flex; gap: 8px;",
          textInput(
            "ob_custom_competitor", NULL,
            placeholder = "Brand name...",
            width = "100%"
          ),
          actionButton(
            "ob_add_custom_competitor",
            icon("plus"),
            style = "background: #667eea; color: white; border: none;
                     border-radius: 8px; padding: 8px 14px; font-size: 14px;"
          )
        )
      ),
      uiOutput("setup_time_estimate_ui"),
      div(
        style = "display: flex; gap: 10px; margin-top: 20px;",
        actionButton(
          "ob_step4_back", "Back",
          style = "flex: 1; height: 44px; background: transparent; color: #9E9E9E;
                   border: 2px solid #e2e8f0; border-radius: 8px; font-weight: 600;"
        ),
        actionButton(
          "ob_step4_next", "Continue",
          style = "flex: 2; height: 44px; background: #1A1A1A; color: #D4A843;
                   border: 2px solid #D4A843; border-radius: 8px; font-weight: 600;
                   font-size: 15px;"
        )
      )
    )
  )
}

output$step4_alert <- renderUI({ NULL })

observeEvent(competitor_lookup_trigger(), {
  req(competitor_lookup_trigger() > 0)
  req(onboarding_data$brand_name, onboarding_data$industry)
  
  competitor_loading(TRUE)
  selected_competitors(character(0))
  
  brand         <- onboarding_data$brand_name
  industry      <- onboarding_data$industry
  reach         <- onboarding_data$brand_reach
  country       <- onboarding_data$reach_country
  region        <- onboarding_data$reach_region
  reach_context <- build_reach_context(reach, country, region)
  
  future_promise({
    reach_str <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
    
    prompt <- paste0(
      "List the top 8 competitors for the brand '", brand, "' in the ", industry,
      " industry", reach_str, ". ",
      "Return ONLY brand names separated by semi-colons. No numbers, no descriptions, ",
      "no extra text. Example format: Brand A; Brand B; Brand C"
    )
    
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(
        model       = "gpt-4.1-mini",
        messages    = list(list(role = "user", content = prompt)),
        temperature = 0.2,
        max_tokens  = 100
      )) |>
      req_perform()
    
    parsed  <- resp_body_json(resp)
    raw_txt <- parsed$choices[[1]]$message$content
    
    raw_txt |>
      strsplit(";") |>
      unlist() |>
      trimws() |>
      (\(x) x[nchar(x) > 0])() |>
      head(8)
    
  }) %...>% (function(comps) {
    suggested_competitors(comps)
    selected_competitors(head(comps, 5))
    competitor_loading(FALSE)
  }) %...!% (function(err) {
    suggested_competitors(character(0))
    selected_competitors(character(0))
    competitor_loading(FALSE)
    showNotification("Could not look up competitors automatically.",
                     type = "warning", duration = 5)
  })
})

output$competitor_selection_ui <- renderUI({
  if (competitor_loading()) {
    return(div(
      style = "text-align: center; padding: 24px;",
      tags$i(class = "fa fa-spinner fa-spin",
             style = "color: #667eea; font-size: 20px;"),
      p(style = "color: #a0aec0; margin-top: 8px; font-size: 13px;",
        "Finding your top competitors...")
    ))
  }
  
  all_comps <- c(suggested_competitors(),
                 setdiff(selected_competitors(), suggested_competitors()))
  selected  <- selected_competitors()
  
  if (length(all_comps) == 0) {
    return(div(
      style = "text-align: center; padding: 20px; color: #a0aec0;",
      p("No suggestions found. Add competitors manually below.")
    ))
  }
  
  div(
    tags$label(
      style = "font-weight: 600; font-size: 13px; color: #4a5568;
               text-transform: uppercase; letter-spacing: 0.5px;
               margin-bottom: 4px; display: block;",
      "Select competitors to track"
    ),
    p(style = "font-size: 12px; color: #a0aec0; margin-bottom: 12px;",
      paste0(length(selected), " selected, click to toggle")),
    
    div(
      style = "display: flex; flex-wrap: wrap; gap: 8px;",
      lapply(all_comps, function(comp) {
        is_selected <- comp %in% selected
        div(
          style = paste0(
            "padding: 8px 14px; border-radius: 20px; cursor: pointer; font-size: 13px; ",
            "font-weight: 500; transition: all 0.15s ease; user-select: none; ",
            "display: flex; align-items: center; gap: 6px; ",
            if (is_selected) {
              "background: #667eea; color: white; border: 2px solid #667eea;"
            } else {
              "background: white; color: #4a5568; border: 2px solid #e2e8f0;"
            }
          ),
          onclick = sprintf(
            "Shiny.setInputValue('ob_toggle_competitor', '%s', {priority: 'event'})",
            comp
          ),
          if (is_selected) icon("check", style = "font-size: 11px;"),
          comp
        )
      })
    )
  )
})

observeEvent(input$ob_toggle_competitor, {
  comp    <- input$ob_toggle_competitor
  current <- selected_competitors()
  if (comp %in% current) {
    selected_competitors(setdiff(current, comp))
  } else {
    selected_competitors(c(current, comp))
  }
})

observeEvent(input$ob_add_custom_competitor, {
  custom <- trimws(input$ob_custom_competitor)
  if (nchar(custom) < 2) return()
  if (!tolower(custom) %in% tolower(suggested_competitors())) {
    suggested_competitors(c(suggested_competitors(), custom))
  }
  if (!tolower(custom) %in% tolower(selected_competitors())) {
    selected_competitors(c(selected_competitors(), custom))
  }
  updateTextInput(session, "ob_custom_competitor", value = "")
})

observeEvent(input$ob_step4_back, { onboarding_step(3) })

observeEvent(input$ob_step4_next, {
  onboarding_data$competitors <- selected_competitors()
  prompt_lookup_trigger(prompt_lookup_trigger() + 1)
  onboarding_step(5)
})

# ============================================
# Step 5: Prompts
# ============================================

prompt_lookup_trigger <- reactiveVal(0)
prompt_loading        <- reactiveVal(FALSE)
suggested_prompts     <- reactiveVal(character(0))
selected_prompts      <- reactiveVal(character(0))

onboarding_step5_ui <- function() {
  div(
    class = "login-container",
    style = "max-width: 620px;",
    div(
      class = "login-header",
      h2("What are people searching for?"),
      p("These are prompts your brand would want to appear in, select the ones most relevant to you or choose your own.")
    ),
    div(
      class = "login-body",
      uiOutput("step5_alert"),
      uiOutput("prompt_selection_ui"),
      
      div(
        style = "margin-top: 16px;",
        tags$label(
          style = "font-weight: 600; font-size: 13px; color: #4a5568;
                   margin-bottom: 8px; display: block;",
          "Add a prompt manually"
        ),
        div(
          style = "display: flex; gap: 8px;",
          textInput(
            "ob_custom_prompt", NULL,
            placeholder = "e.g. What are the best running shoes?",
            width = "100%"
          ),
          actionButton(
            "ob_add_custom_prompt",
            icon("plus"),
            style = "background: #667eea; color: white; border: none;
                     border-radius: 8px; padding: 8px 14px; font-size: 14px;"
          )
        )
      ),
      uiOutput("setup_time_estimate_ui"),
      div(
        style = "display: flex; gap: 10px; margin-top: 20px;",
        actionButton(
          "ob_step5_back", "Back",
          style = "flex: 1; height: 44px; background: transparent; color: #9E9E9E;
                   border: 2px solid #e2e8f0; border-radius: 8px; font-weight: 600;"
        ),
        actionButton(
          "ob_step5_next", "Continue",
          style = "flex: 2; height: 44px; background: #1A1A1A; color: #D4A843;
                   border: 2px solid #D4A843; border-radius: 8px; font-weight: 600;
                   font-size: 15px;"
        )
      )
    )
  )
}

output$step5_alert <- renderUI({ NULL })

observeEvent(prompt_lookup_trigger(), {
  req(prompt_lookup_trigger() > 0)
  req(onboarding_data$brand_name, onboarding_data$industry)
  
  prompt_loading(TRUE)
  selected_prompts(character(0))
  
  brand         <- onboarding_data$brand_name
  industry      <- onboarding_data$industry
  reach         <- onboarding_data$brand_reach
  country       <- onboarding_data$reach_country
  region        <- onboarding_data$reach_region
  postcode      <- onboarding_data$reach_postcode
  reach_context <- build_reach_context(reach, country, region, postcode)
  
  future_promise({
    reach_str   <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
    near_me_str <- build_near_me_suffix(reach, country, postcode)
    
    location_str <- if (nzchar(near_me_str)) {
      paste0(" near ", near_me_str)
    } else if (nzchar(reach_str)) {
      reach_str
    } else {
      ""
    }
    
    prompt <- paste0(
      "Generate 10 search prompts that a potential customer would type into an AI assistant ",
      "when looking for a brand, product, or service in the ", industry, " industry",
      location_str, ". ",
      "The prompts must:\n",
      "- Be phrased as natural questions a buyer would ask\n",
      "- Be specific enough that a brand name would appear in the answer\n",
      "- Cover different buying intents (best, recommended, cheapest, near me, reviews, etc.)\n",
      "- NOT mention any specific brand names\n",
      "- Include the location context where relevant\n\n",
      "Return ONLY the 10 prompts separated by semi-colons. ",
      "No numbers, no explanations, no brand names."
    )
    
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ) |>
      req_body_json(list(
        model       = "gpt-4o-mini",
        messages    = list(list(role = "user", content = prompt)),
        temperature = 0.5,
        max_tokens  = 400
      )) |>
      req_perform()
    
    parsed  <- resp_body_json(resp)
    raw_txt <- sanitise_text(parsed$choices[[1]]$message$content)
    
    raw_txt |>
      strsplit(";") |>
      unlist() |>
      trimws() |>
      (\(x) gsub("^[0-9]+[.)\\s]+", "", x))() |>
      trimws() |>
      (\(x) x[grepl("\\?$", x)])() |>
      (\(x) x[nchar(x) > 10])() |>
      head(10)
    
  }) %...>% (function(prompts) {
    suggested_prompts(prompts)
    selected_prompts(head(prompts, 5))
    prompt_loading(FALSE)
  }) %...!% (function(err) {
    suggested_prompts(character(0))
    selected_prompts(character(0))
    prompt_loading(FALSE)
    showNotification("Could not look up prompts automatically.",
                     type = "warning", duration = 5)
  })
})

output$prompt_selection_ui <- renderUI({
  if (prompt_loading()) {
    return(div(
      style = "text-align: center; padding: 24px;",
      tags$i(class = "fa fa-spinner fa-spin",
             style = "color: #667eea; font-size: 20px;"),
      p(style = "color: #a0aec0; margin-top: 8px; font-size: 13px;",
        "Finding the most relevant prompts for your industry...")
    ))
  }
  
  all_prompts <- c(suggested_prompts(),
                   setdiff(selected_prompts(), suggested_prompts()))
  selected    <- selected_prompts()
  
  if (length(all_prompts) == 0) {
    return(div(
      style = "text-align: center; padding: 20px; color: #a0aec0;",
      p("No suggestions found. Add prompts manually below.")
    ))
  }
  
  div(
    tags$label(
      style = "font-weight: 600; font-size: 13px; color: #4a5568;
               text-transform: uppercase; letter-spacing: 0.5px;
               margin-bottom: 4px; display: block;",
      "Select prompts to track"
    ),
    p(style = "font-size: 12px; color: #a0aec0; margin-bottom: 12px;",
      paste0(length(selected), " of ", length(all_prompts),
             " selected, click to toggle")),
    
    div(
      style = "display: flex; flex-direction: column; gap: 8px;",
      lapply(all_prompts, function(prompt_text) {
        is_selected <- prompt_text %in% selected
        
        div(
          style = paste0(
            "padding: 10px 16px; border-radius: 10px; cursor: pointer; ",
            "font-size: 13px; transition: all 0.15s ease; user-select: none; ",
            "display: flex; align-items: center; gap: 10px; ",
            if (is_selected) {
              "background: linear-gradient(135deg, rgba(102,126,234,0.1),
               rgba(118,75,162,0.1)); border: 2px solid #667eea; color: #4a5568;"
            } else {
              "background: white; border: 2px solid #e2e8f0; color: #718096;"
            }
          ),
          onclick = sprintf(
            "Shiny.setInputValue('ob_toggle_prompt', %s, {priority: 'event'})",
            jsonlite::toJSON(prompt_text, auto_unbox = TRUE)
          ),
          div(
            style = paste0(
              "flex: 0 0 20px; width: 20px; height: 20px; border-radius: 5px; ",
              "display: flex; align-items: center; justify-content: center; ",
              "font-size: 11px; transition: all 0.15s; ",
              if (is_selected) {
                "background: #667eea; color: white; border: 2px solid #667eea;"
              } else {
                "background: white; border: 2px solid #cbd5e0;"
              }
            ),
            if (is_selected) icon("check")
          ),
          div(style = "flex: 1; line-height: 1.4;", prompt_text)
        )
      })
    )
  )
})

observeEvent(input$ob_toggle_prompt, {
  prompt_text <- input$ob_toggle_prompt
  current     <- selected_prompts()
  if (prompt_text %in% current) {
    selected_prompts(setdiff(current, prompt_text))
  } else {
    selected_prompts(c(current, prompt_text))
  }
})

observeEvent(input$ob_add_custom_prompt, {
  custom <- trimws(input$ob_custom_prompt)
  if (nchar(custom) < 5) return()
  if (!tolower(custom) %in% tolower(suggested_prompts())) {
    suggested_prompts(c(suggested_prompts(), custom))
  }
  if (!tolower(custom) %in% tolower(selected_prompts())) {
    selected_prompts(c(selected_prompts(), custom))
  }
  updateTextInput(session, "ob_custom_prompt", value = "")
})

observeEvent(input$ob_step5_back, { onboarding_step(4) })

observeEvent(input$ob_step5_next, {
  onboarding_data$prompts <- selected_prompts()
  onboarding_step(6)
})

# ============================================
# Step 6: Personas
# ============================================

# Standard persona names — first 4 shown as quick-select cards
ONBOARDING_PERSONA_SUGGESTIONS <- list(
  list(
    name = "Young Professional",
    desc = "I am an urban professional aged 22-32 with a college degree earning between $45,000 and $80,000 per year. I am career focused and either single or newly in a relationship.",
    icon = "briefcase"
  ),
  list(
    name = "Family Focused",
    desc = "I am a married parent aged 30-45 with two or more children. My household income is between $60,000 and $100,000 per year and I live in the suburbs and prioritise value for money.",
    icon = "house-chimney"
  ),
  list(
    name = "Budget Conscious",
    desc = "I am a price-sensitive shopper with a household income under $40,000 per year. I always look for the best deal and prioritise affordability over brand prestige.",
    icon = "piggy-bank"
  ),
  list(
    name = "Luxury Seeker",
    desc = "I am a high earner aged 30-55 with a household income over $150,000 per year. I am brand conscious and always prioritise quality and prestige over price.",
    icon = "gem"
  )
)

ob_selected_personas  <- reactiveVal(list())   # list of list(name, desc)
ob_custom_persona_err <- reactiveVal(NULL)

onboarding_step6_ui <- function() {
  div(
    class = "login-container",
    style = "max-width: 680px;",
    div(
      class = "login-header",
      h2("Add marketing personas"),
      p("Score your brand through the eyes of specific customer segments.
         Select from our standard personas, add your own, or skip for now.")
    ),
    div(
      class = "login-body",
      
      # ── Standard persona cards ──────────────────────────────────────────
      tags$label(
        style = "font-weight: 600; font-size: 12px; color: #718096;
                 text-transform: uppercase; letter-spacing: 0.5px;
                 margin-bottom: 10px; display: block;",
        "Standard personas, click to add"
      ),
      uiOutput("ob_persona_cards_ui"),
      
      hr(style = "border-color: #f0f0f0; margin: 20px 0;"),
      
      # ── Selected personas list ──────────────────────────────────────────
      uiOutput("ob_selected_personas_ui"),
      
      hr(style = "border-color: #f0f0f0; margin: 20px 0;"),
      
      # ── Custom persona creator ──────────────────────────────────────────
      tags$label(
        style = "font-weight: 600; font-size: 12px; color: #718096;
                 text-transform: uppercase; letter-spacing: 0.5px;
                 margin-bottom: 10px; display: block;",
        "Create a custom persona"
      ),
      uiOutput("ob_custom_persona_err_ui"),
      textInput(
        "ob_custom_persona_name", NULL,
        placeholder = "Persona name e.g. 'Eco Conscious Millennial'",
        width = "100%"
      ),
      tags$textarea(
        id          = "ob_custom_persona_desc",
        class       = "form-control",
        rows        = 3,
        placeholder = "Describe in first person, e.g. 'I am a...'",
        style       = "font-size: 13px; resize: vertical; margin-bottom: 8px;"
      ),
      actionButton(
        "ob_add_custom_persona_btn",
        "Add Persona",
        icon  = icon("plus"),
        style = "background: #8E44AD; color: white; border: none;
                 border-radius: 8px; padding: 8px 18px; font-weight: 600;
                 font-size: 13px; margin-bottom: 8px;"
      ),
      
      hr(style = "border-color: #f0f0f0; margin: 20px 0;"),
      
      # ── Navigation ─────────────────────────────────────────────────────
      uiOutput("setup_time_estimate_ui"),
      div(
        style = "display: flex; gap: 10px;",
        actionButton(
          "ob_step6_back", "Back",
          style = "flex: 1; height: 44px; background: transparent; color: #9E9E9E;
                   border: 2px solid #e2e8f0; border-radius: 8px; font-weight: 600;"
        ),
        actionButton(
          "ob_step6_skip", "Skip for now",
          style = "flex: 1; height: 44px; background: transparent; color: #718096;
                   border: 2px solid #e2e8f0; border-radius: 8px; font-weight: 600;"
        ),
        actionButton(
          "ob_step6_finish", "Complete Setup",
          icon  = icon("check"),
          style = "flex: 2; height: 44px; background: #1A1A1A; color: #D4A843;
                   border: 2px solid #D4A843; border-radius: 8px; font-weight: 600;
                   font-size: 15px;"
        )
      )
    )
  )
}

# Standard persona suggestion cards
output$ob_persona_cards_ui <- renderUI({
  selected <- ob_selected_personas()
  sel_names <- sapply(selected, `[[`, "name")
  
  div(
    style = "display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 4px;",
    lapply(ONBOARDING_PERSONA_SUGGESTIONS, function(p) {
      is_sel <- p$name %in% sel_names
      
      div(
        style = paste0(
          "flex: 1; min-width: 130px; border-radius: 12px; padding: 14px 12px; ",
          "text-align: center; cursor: pointer; transition: all 0.2s ease; ",
          if (is_sel) {
            "border: 2px solid #8E44AD;
             background: linear-gradient(135deg, rgba(142,68,173,0.08),
             rgba(142,68,173,0.04));
             box-shadow: 0 0 0 3px rgba(142,68,173,0.12);"
          } else {
            "border: 2px solid #e2e8f0; background: white;"
          }
        ),
        onclick = sprintf(
          "Shiny.setInputValue('ob_toggle_std_persona', '%s', {priority: 'event'})",
          p$name
        ),
        div(
          style = paste0(
            "font-size: 22px; margin-bottom: 8px; ",
            if (is_sel) "color: #8E44AD;" else "color: #a0aec0;"
          ),
          icon(p$icon)
        ),
        div(
          style = paste0(
            "font-size: 13px; font-weight: 700; margin-bottom: 2px; ",
            if (is_sel) "color: #8E44AD;" else "color: #2d3748;"
          ),
          p$name
        ),
        if (is_sel) {
          div(
            style = "margin-top: 6px;",
            tags$span(
              style = "background: #8E44AD; color: white; font-size: 10px;
                       padding: 2px 8px; border-radius: 10px; font-weight: 600;",
              icon("check", style = "font-size: 9px; margin-right: 3px;"),
              "Added"
            )
          )
        }
      )
    })
  )
})

# Toggle a standard persona on/off
observeEvent(input$ob_toggle_std_persona, {
  name    <- input$ob_toggle_std_persona
  current <- ob_selected_personas()
  names   <- sapply(current, `[[`, "name")
  
  if (name %in% names) {
    # Remove
    ob_selected_personas(current[!names %in% name])
  } else {
    # Add — look up descriptor from suggestion list
    match <- Filter(function(p) p$name == name, ONBOARDING_PERSONA_SUGGESTIONS)
    if (length(match) > 0) {
      ob_selected_personas(c(current, list(match[[1]])))
    }
  }
})

# Add custom persona
output$ob_custom_persona_err_ui <- renderUI({
  err <- ob_custom_persona_err()
  if (!is.null(err)) div(class = "alert alert-danger", style = "margin-bottom: 8px;", err)
})

observeEvent(input$ob_add_custom_persona_btn, {
  name <- trimws(input$ob_custom_persona_name)
  desc <- trimws(input$ob_custom_persona_desc)
  
  if (nchar(name) < 2) {
    ob_custom_persona_err("Please enter a persona name.")
    return()
  }
  if (nchar(desc) < 10) {
    ob_custom_persona_err("Please enter a description (at least 10 characters).")
    return()
  }
  
  current   <- ob_selected_personas()
  cur_names <- sapply(current, `[[`, "name")
  
  if (tolower(name) %in% tolower(cur_names)) {
    ob_custom_persona_err("A persona with that name is already added.")
    return()
  }
  
  ob_custom_persona_err(NULL)
  ob_selected_personas(c(current, list(list(name = name, desc = desc, icon = "user-pen"))))
  updateTextInput(session, "ob_custom_persona_name", value = "")
  shinyjs::runjs("document.getElementById('ob_custom_persona_desc').value = ''")
})

# Display selected personas
output$ob_selected_personas_ui <- renderUI({
  selected <- ob_selected_personas()
  
  if (length(selected) == 0) {
    return(div(
      style = "text-align: center; padding: 16px; color: #a0aec0; 
               border: 1px dashed #e2e8f0; border-radius: 10px;",
      icon("users", style = "margin-bottom: 6px; color: #e2e8f0; font-size: 20px;"),
      p(style = "margin: 0; font-size: 13px;",
        "No personas selected yet, add some above or skip this step.")
    ))
  }
  
  div(
    tags$label(
      style = "font-weight: 600; font-size: 12px; color: #718096;
               text-transform: uppercase; letter-spacing: 0.5px;
               margin-bottom: 8px; display: block;",
      paste0(length(selected), " persona", if (length(selected) != 1) "s" else "",
             " selected")
    ),
    div(
      style = "display: flex; flex-direction: column; gap: 6px;",
      lapply(seq_along(selected), function(i) {
        p <- selected[[i]]
        div(
          style = "display: flex; align-items: center; gap: 10px; padding: 10px 12px;
                   border-radius: 10px; background: rgba(142,68,173,0.04);
                   border: 1px solid rgba(142,68,173,0.15);",
          div(
            style = "flex: 0 0 32px; width: 32px; height: 32px; border-radius: 8px;
                     background: rgba(142,68,173,0.12); display: flex;
                     align-items: center; justify-content: center; color: #8E44AD;",
            icon(if (!is.null(p$icon)) p$icon else "users", style = "font-size: 13px;")
          ),
          div(
            style = "flex: 1; min-width: 0;",
            div(style = "font-size: 13px; font-weight: 600; color: #2d3748;",
                p$name),
            div(style = "font-size: 11px; color: #a0aec0; white-space: nowrap;
                         overflow: hidden; text-overflow: ellipsis;",
                substr(p$desc, 1, 80), if (nchar(p$desc) > 80) "...")
          ),
          tags$button(
            style = "flex: 0 0 auto; background: none; border: none; color: #a0aec0;
                     cursor: pointer; font-size: 14px; padding: 2px 6px;
                     border-radius: 4px;",
            onclick = sprintf(
              "Shiny.setInputValue('ob_remove_persona', '%s', {priority: 'event'})",
              p$name
            ),
            icon("times")
          )
        )
      })
    )
  )
})

# Remove a persona from selected list
observeEvent(input$ob_remove_persona, {
  name    <- input$ob_remove_persona
  current <- ob_selected_personas()
  ob_selected_personas(Filter(function(p) p$name != name, current))
})

observeEvent(input$ob_step6_back, { onboarding_step(5) })

# ============================================
# Shared finish logic — called by both skip and finish
# ============================================

.run_finish_onboarding <- function(personas) {
  message("=== .run_finish_onboarding START ===")
  message(sprintf("  login_id: %s, brand: %s", rv$login_id, onboarding_data$brand_name))
  
  brand_name  <- onboarding_data$brand_name
  brand_reach <- onboarding_data$brand_reach
  industry    <- onboarding_data$industry
  login_id    <- rv$login_id
  competitors <- selected_competitors()
  prompts     <- selected_prompts()
  
  country <- if (is.null(onboarding_data$reach_country) ||
                 !nzchar(onboarding_data$reach_country %||% "")) {
    NA_character_
  } else { onboarding_data$reach_country }
  
  region <- if (is.null(onboarding_data$reach_region) ||
                !nzchar(onboarding_data$reach_region %||% "")) {
    NA_character_
  } else { onboarding_data$reach_region }
  
  postcode <- if (is.null(onboarding_data$reach_postcode) ||
                  !nzchar(onboarding_data$reach_postcode %||% "")) {
    NA_character_
  } else { onboarding_data$reach_postcode }
  
  db_success     <- FALSE
  brand_id       <- NULL
  comp_brand_ids <- list()
  prompt_ids     <- list()
  
  tryCatch({
    
    # --- Save / update main brand ---
    existing <- dbGetQuery(pool,
                           "SELECT brand_id FROM dim_brand WHERE lower(brand_name) = lower($1)",
                           params = list(brand_name))
    
    if (nrow(existing) == 0) {
      brand_row <- dbGetQuery(pool,
                              "INSERT INTO dim_brand
           (brand_name, brand_reach, reach_country, reach_region, reach_postcode, industry)
         VALUES ($1, $2, $3, $4, $5, $6) RETURNING brand_id",
                              params = list(brand_name, brand_reach, country, region, postcode, industry))
      brand_id <- brand_row$brand_id[1]
    } else {
      brand_id <- existing$brand_id[1]
      dbExecute(pool,
                "UPDATE dim_brand
         SET brand_reach = $1, reach_country = $2, reach_region = $3,
             reach_postcode = $4, industry = $5
         WHERE brand_id = $6",
                params = list(brand_reach, country, region, postcode, industry, brand_id))
    }
    
    # --- Link main brand ---
    dbExecute(pool,
              "INSERT INTO fact_user_brands_tracked
         (login_id, brand_id, main_brand_flag, date_valid_from, industry)
       VALUES ($1, $2, TRUE, $3, $4)
       ON CONFLICT (login_id, brand_id, date_valid_from) DO UPDATE
         SET industry = EXCLUDED.industry",
              params = list(login_id, brand_id, Sys.Date(), industry))
    
    # --- Save competitors ---
    if (length(competitors) > 0) {
      for (comp_name in competitors) {
        existing_comp <- dbGetQuery(pool,
                                    "SELECT brand_id FROM dim_brand WHERE lower(brand_name) = lower($1)",
                                    params = list(comp_name))
        
        if (nrow(existing_comp) == 0) {
          comp_row <- dbGetQuery(pool,
                                 "INSERT INTO dim_brand
               (brand_name, brand_reach, reach_country, reach_region, reach_postcode, industry)
             VALUES ($1, $2, $3, $4, $5, $6) RETURNING brand_id",
                                 params = list(comp_name, brand_reach, country, region, postcode, industry))
          comp_id <- comp_row$brand_id[1]
        } else {
          comp_id <- existing_comp$brand_id[1]
        }
        
        dbExecute(pool,
                  "INSERT INTO fact_user_brands_tracked
             (login_id, brand_id, main_brand_flag, date_valid_from, industry)
           VALUES ($1, $2, FALSE, $3, $4)
           ON CONFLICT (login_id, brand_id, date_valid_from) DO UPDATE
             SET industry = EXCLUDED.industry",
                  params = list(login_id, comp_id, Sys.Date(), industry))
        
        comp_brand_ids[[comp_name]] <- comp_id
      }
    }
    
    # --- Save prompts ---
    if (length(prompts) > 0) {
      for (prompt_text in prompts) {
        existing_q <- dbGetQuery(pool,
                                 "SELECT query_id FROM dim_query WHERE query_string = $1",
                                 params = list(prompt_text))
        
        if (nrow(existing_q) == 0) {
          existing_q <- dbGetQuery(pool,
                                   "INSERT INTO dim_query (query_string) VALUES ($1) RETURNING query_id",
                                   params = list(prompt_text))
        }
        
        query_id <- existing_q$query_id[1]
        prompt_ids[[prompt_text]] <- query_id
        
        dbExecute(pool,
                  "INSERT INTO fact_user_queries_tracked
             (login_id, query_id, date_valid_from)
           VALUES ($1, $2, $3)
           ON CONFLICT (login_id, query_id, date_valid_from) DO NOTHING",
                  params = list(login_id, query_id, Sys.Date()))
        
        dbExecute(pool,
                  "INSERT INTO dim_brand_query (brand_id, query_id, date_added)
           VALUES ($1, $2, $3)
           ON CONFLICT (brand_id, query_id) DO NOTHING",
                  params = list(brand_id, query_id, Sys.Date()))
        
        for (comp_id in comp_brand_ids) {
          dbExecute(pool,
                    "INSERT INTO dim_brand_query (brand_id, query_id, date_added)
             VALUES ($1, $2, $3)
             ON CONFLICT (brand_id, query_id) DO NOTHING",
                    params = list(comp_id, query_id, Sys.Date()))
        }
      }
    }
    
    # --- Save personas ---
    if (length(personas) > 0) {
      for (persona in personas) {
        pname <- persona$name
        pdesc <- persona$desc
        
        # Check if standard persona already exists
        pid_row <- dbGetQuery(pool,
                              "SELECT profile_id FROM dim_customer_profile
           WHERE profile_name = $1 AND is_standard = TRUE",
                              params = list(pname))
        
        if (nrow(pid_row) == 0) {
          # Create as custom persona
          pid_row <- dbGetQuery(pool,
                                "INSERT INTO dim_customer_profile
               (profile_name, profile_descriptor, is_standard, created_by_login, date_created)
             VALUES ($1, $2, FALSE, $3, $4)
             RETURNING profile_id",
                                params = list(pname, pdesc, login_id, Sys.Date()))
        }
        
        pid <- pid_row$profile_id[1]
        
        dbExecute(pool,
                  "INSERT INTO fact_user_profiles_tracked
             (login_id, profile_id, date_valid_from)
           VALUES ($1, $2, $3)
           ON CONFLICT (login_id, profile_id, date_valid_from) DO NOTHING",
                  params = list(login_id, pid, Sys.Date()))
      }
    }
    
    # --- Dedupe brands ---
    dedupe_user_brands_tracked(login_id)
    
    # --- Mark onboarding complete ---
    dbExecute(pool,
              "UPDATE dim_user SET onboarding_complete = TRUE WHERE login_id = $1",
              params = list(login_id))
    
    db_success <- TRUE
    message("=== DB SAVE SUCCESS ===")
  }, error = function(e) {
    message("=== DB SAVE ERROR: ", e$message, " ===")
    showNotification(
      paste("Warning: Some data may not have saved:", e$message),
      type = "warning", duration = 10
    )
  })
  
  message(sprintf("=== Setting onboarding_complete, db_success=%s ===", db_success))
  
  # --- Update reactive values immediately ---
  # This MUST be the last thing — always runs
  if (!is.null(brand_id)) rv$brand_id <- brand_id
  rv$brand_name          <- brand_name
  rv$onboarding_complete <- TRUE
  rv$brands_refresh      <- rv$brands_refresh + 1
  rv$queries_refresh     <- rv$queries_refresh + 1
  
  message("=== rv$onboarding_complete set to TRUE ===")
  
  showNotification(
    paste0("Welcome, ", brand_name, "! Calculating your scores in the background..."),
    type = "message", duration = 8
  )
  
  if (!db_success || is.null(brand_id)) {
    showNotification(
      "There was an issue saving your setup. Please check Account settings.",
      type = "warning", duration = 10
    )
    return()
  }
  
  # --- Queue scoring jobs ---
  
  # Score main brand
  dbExecute(pool,
            "INSERT INTO dim_job_queue (job_type, login_id, payload)
     VALUES ('score_brand', $1, $2)",
            params = list(login_id, toJSON(list(
              brand_name = brand_name,
              brand_id   = brand_id
            ), auto_unbox = TRUE)))
  
  # Score main brand prompts
  if (length(prompts) > 0) {
    dbExecute(pool,
              "INSERT INTO dim_job_queue (job_type, login_id, payload)
       VALUES ('score_brand_prompts', $1, $2)",
              params = list(login_id, toJSON(list(
                brand_id = brand_id,
                prompts  = prompt_ids
              ), auto_unbox = TRUE)))
  }
  
  # Score competitors
  if (length(competitors) > 0) {
    for (comp_name in competitors) {
      comp_id <- comp_brand_ids[[comp_name]]
      dbExecute(pool,
                "INSERT INTO dim_job_queue (job_type, login_id, payload)
         VALUES ('score_competitor', $1, $2)",
                params = list(login_id, toJSON(list(
                  brand_name = comp_name,
                  brand_id   = comp_id,
                  prompts    = prompt_ids
                ), auto_unbox = TRUE)))
    }
  }
  
  # Score personas
  if (length(personas) > 0) {
    persona_ids <- dbGetQuery(pool,
                              "SELECT profile_id FROM fact_user_profiles_tracked
       WHERE login_id = $1
         AND date_valid_from <= CURRENT_DATE
         AND (date_valid_to IS NULL OR date_valid_to >= CURRENT_DATE)",
                              params = list(login_id))$profile_id
    
    for (pid in persona_ids) {
      dbExecute(pool,
                "INSERT INTO dim_job_queue (job_type, login_id, payload)
         VALUES ('score_profile', $1, $2)",
                params = list(login_id, toJSON(list(
                  profile_id = pid
                ), auto_unbox = TRUE)))
    }
  }
}

observeEvent(input$ob_step6_skip, {
  req(rv$logged_in, rv$login_id)
  ob_selected_personas(list())
  .run_finish_onboarding(list())
})

observeEvent(input$ob_step6_finish, {
  req(rv$logged_in, rv$login_id)
  .run_finish_onboarding(ob_selected_personas())
})


# ============================================
# Helpers
# ============================================

get_country_list <- function() {
  c(
    "Afghanistan", "Albania", "Algeria", "Argentina", "Australia", "Austria",
    "Bangladesh", "Belgium", "Bolivia", "Brazil", "Canada", "Chile", "China",
    "Colombia", "Croatia", "Czech Republic", "Denmark", "Ecuador", "Egypt",
    "Ethiopia", "Finland", "France", "Germany", "Ghana", "Greece", "Guatemala",
    "Hungary", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel",
    "Italy", "Japan", "Jordan", "Kenya", "Malaysia", "Mexico", "Morocco",
    "Netherlands", "New Zealand", "Nigeria", "Norway", "Pakistan", "Peru",
    "Philippines", "Poland", "Portugal", "Romania", "Russia", "Saudi Arabia",
    "South Africa", "South Korea", "Spain", "Sri Lanka", "Sweden", "Switzerland",
    "Taiwan", "Tanzania", "Thailand", "Turkey", "Uganda", "Ukraine",
    "United Arab Emirates", "United Kingdom", "United States", "Uruguay",
    "Venezuela", "Vietnam", "Zimbabwe"
  )
}