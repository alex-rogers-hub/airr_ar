# ============================================
# Customer personas — Enterprise only
# ============================================

STANDARD_PROFILE_NAMES <- c(
  "Athletic & Active", "Young Professional", "Family Focused",
  "Budget Conscious", "Luxury Seeker", "Tech Enthusiast",
  "Senior", "Student", "Small Business Owner", "Health & Wellness",
  "Eco Conscious", "Urban Millennial", "Suburban Parent",
  "High Earner", "Retiree"
)

# ============================================
# Access gate
# ============================================

output$profiles_access_gate <- renderUI({
  req(rv$logged_in)
  
  sub <- user_subscription()
  
  if (sub$subscription_name != "Enterprise") {
    return(
      div(
        style = "text-align: center; padding: 60px 20px;",
        div(
          style = "background: white; border-radius: 16px; padding: 50px 40px; 
                   max-width: 500px; margin: 0 auto;
                   box-shadow: 0 4px 20px rgba(0,0,0,0.08);
                   border-top: 4px solid #8E44AD;",
          icon("crown", class = "fa-3x", style = "color: #8E44AD; margin-bottom: 20px;"),
          h3(style = "font-weight: 700; color: #2d3748; margin-bottom: 10px;",
             "Enterprise Feature"),
          p(style = "color: #718096; font-size: 14px; line-height: 1.6; margin-bottom: 24px;",
            "Customer personas allow you to score your brand and prompts ",
            "through the eyes of specific customer segments. ",
            "Upgrade to Enterprise to unlock this feature."),
          actionButton("upgrade_from_profiles", "View Plans",
                       icon = icon("arrow-up"),
                       style = "background: #8E44AD; color: white; border: none; 
                                border-radius: 8px; padding: 10px 28px; 
                                font-weight: 600; font-size: 14px;")
        )
      )
    )
  }
  
  profiles_main_ui()
})

observeEvent(input$upgrade_from_profiles, {
  shinyjs::click("upgrade_btn")
})

# ============================================
# Main profiles UI (enterprise only)
# ============================================

profiles_main_ui <- function() {
  div(
    fluidRow(
      # Left: profile management
      column(
        width = 4,
        div(
          class = "box",
          style = "border-radius: 12px; padding: 20px; 
                   box-shadow: 0 2px 12px rgba(0,0,0,0.06);",
          
          div(
            style = "display: flex; justify-content: space-between; 
                     align-items: center; margin-bottom: 16px;",
            h4(style = "margin: 0; font-weight: 600; color: #2d3748;",
               "Your personas"),
            uiOutput("profile_slot_badge")
          ),
          
          # Add standard profile
          div(
            style = "margin-bottom: 4px;",       # reduced — notice sits below
            selectInput(
              "add_standard_profile_select",
              NULL,
              choices = c("Add a standard persona..." = "", STANDARD_PROFILE_NAMES),
              width = "100%"
            ),
            # Timing notice for standard persona
            uiOutput("profile_standard_timing_notice"),
            div(style = "margin-top: 8px;",
                actionButton(
                  "add_standard_profile_btn",
                  "Add Selected",
                  icon = icon("plus"),
                  class = "btn-primary",
                  width = "100%"
                )
            )
          ),
          
          hr(style = "border-color: #f0f0f0; margin: 16px 0;"),
          
          # Custom profile creator
          div(
            style = "margin-bottom: 12px;",
            tags$label(
              style = "font-weight: 600; font-size: 12px; color: #718096; 
                       text-transform: uppercase; letter-spacing: 0.5px;",
              "Or create a custom persona"
            ),
            br(), br(),
            textInput(
              "custom_profile_name", NULL,
              placeholder = "persona name...",
              width = "100%"
            ),
            tags$textarea(
              id = "custom_profile_descriptor",
              class = "form-control",
              rows = 4,
              placeholder = "Describe this customer in first person — e.g. 'I am a...' This will be prefixed to all scoring prompts.",
              style = "font-size: 13px; resize: vertical; margin-bottom: 8px;"
            ),
            # Timing notice for custom persona
            uiOutput("profile_custom_timing_notice"),
            div(style = "margin-top: 8px;",
                actionButton(
                  "add_custom_profile_btn",
                  "Create persona",
                  icon = icon("plus"),
                  class = "btn-primary",
                  width = "100%"
                )
            )
          ),
          
          hr(style = "border-color: #f0f0f0; margin: 16px 0;"),
          
          uiOutput("profile_list_ui")
        )
      ),
      
      # Right: scores for selected profile
      column(
        width = 8,
        uiOutput("profile_scores_ui")
      )
    )
  )
}

# ============================================
# Reactives
# ============================================

profiles_refresh     <- reactiveVal(0)
selected_profile_id  <- reactiveVal(NULL)

user_profiles <- reactive({
  req(rv$logged_in, rv$login_id)
  profiles_refresh()
  
  dbGetQuery(pool, "
    SELECT dcp.profile_id, dcp.profile_name, dcp.profile_descriptor,
           dcp.is_standard, upt.date_valid_from
    FROM fact_user_profiles_tracked upt
    JOIN dim_customer_profile dcp ON dcp.profile_id = upt.profile_id
    WHERE upt.login_id = $1
      AND upt.date_valid_from <= CURRENT_DATE
      AND (upt.date_valid_to IS NULL OR upt.date_valid_to > CURRENT_DATE)
    ORDER BY upt.date_valid_from DESC
  ", params = list(rv$login_id))
})

user_profile_count <- reactive({
  req(rv$logged_in, rv$login_id)
  profiles_refresh()
  nrow(user_profiles())
})

MAX_PROFILES <- 10

# ============================================
# Persona timing notices
# ============================================

output$profile_standard_timing_notice <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  selected_name <- input$add_standard_profile_select
  if (is.null(selected_name) || selected_name == "") return(NULL)
  
  est <- tryCatch(
    estimate_persona_add_time(rv$login_id),
    error = function(e) NULL
  )
  if (is.null(est)) return(NULL)
  
  render_timing_notice(est$time_str, est$breakdown, "persona")
})

output$profile_custom_timing_notice <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  # Show when user has filled in at least a name
  pname <- input$custom_profile_name
  if (is.null(pname) || nchar(trimws(pname)) < 2) return(NULL)
  
  est <- tryCatch(
    estimate_persona_add_time(rv$login_id),
    error = function(e) NULL
  )
  if (is.null(est)) return(NULL)
  
  render_timing_notice(est$time_str, est$breakdown, "persona")
})


# ============================================
# Slot badge
# ============================================

output$profile_slot_badge <- renderUI({
  used      <- user_profile_count()
  remaining <- MAX_PROFILES - used
  
  if (remaining > 0) {
    tags$span(class = "slot-badge available",
              paste0(remaining, " slot", ifelse(remaining != 1, "s", ""), " remaining"))
  } else {
    tags$span(class = "slot-badge full", "No slots remaining")
  }
})

# ============================================
# Profile list
# ============================================

output$profile_list_ui <- renderUI({
  profiles <- user_profiles()
  
  if (nrow(profiles) == 0) {
    return(div(
      style = "text-align: center; padding: 20px; color: #a0aec0;",
      icon("users", class = "fa-2x", style = "margin-bottom: 10px; color: #e2e8f0;"),
      p(style = "font-size: 13px; margin: 0;", "No personas added yet")
    ))
  }
  
  selected <- selected_profile_id()
  
  div(
    lapply(1:nrow(profiles), function(i) {
      pid    <- profiles$profile_id[i]
      pname  <- profiles$profile_name[i]
      is_std <- profiles$is_standard[i]
      is_sel <- !is.null(selected) && selected == pid
      
      div(
        style = paste0(
          "display: flex; align-items: center; padding: 10px 12px; ",
          "border-radius: 8px; cursor: pointer; margin-bottom: 4px; ",
          "transition: all 0.15s ease; border: 2px solid; ",
          if (is_sel) {
            "border-color: #8E44AD; background: rgba(142,68,173,0.06);"
          } else {
            "border-color: transparent; background: #fafaf7;"
          }
        ),
        onclick = sprintf(
          "Shiny.setInputValue('select_profile_id', %d, {priority: 'event'})", pid
        ),
        
        div(
          style = paste0(
            "flex: 0 0 32px; width: 32px; height: 32px; border-radius: 8px; ",
            "display: flex; align-items: center; justify-content: center; ",
            "font-size: 13px; margin-right: 10px; ",
            if (is_sel) {
              "background: #8E44AD; color: white;"
            } else {
              "background: rgba(142,68,173,0.1); color: #8E44AD;"
            }
          ),
          icon(if (is_std) "users" else "user-pen")
        ),
        
        div(
          style = "flex: 1; min-width: 0;",
          div(style = paste0("font-size: 13px; font-weight: ",
                             if (is_sel) "600" else "500",
                             "; color: #2d3748; white-space: nowrap; ",
                             "overflow: hidden; text-overflow: ellipsis;"),
              pname),
          div(style = "font-size: 11px; color: #a0aec0;",
              if (is_std) "Standard" else "Custom")
        ),
        
        # Remove button — stopPropagation so it doesn't also fire select
        tags$button(
          class = "btn-remove",
          style = "flex: 0 0 28px;",
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('remove_profile_id', %d, {priority: 'event'})",
            pid
          ),
          icon("times")
        )
      )
    })
  )
})

# ============================================
# Select profile
# ============================================

observeEvent(input$select_profile_id, {
  selected_profile_id(input$select_profile_id)
})

# ============================================
# Add standard profile
# ============================================

observeEvent(input$add_standard_profile_btn, {
  req(rv$logged_in, rv$login_id)
  
  profile_name <- input$add_standard_profile_select
  if (is.null(profile_name) || profile_name == "") {
    showNotification("Please select a profile to add.", type = "error", duration = 3)
    return()
  }
  
  if (user_profile_count() >= MAX_PROFILES) {
    showNotification("persona limit reached.", type = "error", duration = 3)
    return()
  }
  
  existing <- user_profiles()
  if (profile_name %in% existing$profile_name) {
    showNotification("You're already tracking this profile.", type = "warning", duration = 3)
    return()
  }
  
  pid_row <- dbGetQuery(pool,
                        "SELECT profile_id FROM dim_customer_profile 
                         WHERE profile_name = $1",
                        params = list(profile_name))
  
  if (nrow(pid_row) == 0) {
    showNotification("persona not found.", type = "error", duration = 3)
    return()
  }
  
  tryCatch({
    dbExecute(pool,
              "INSERT INTO fact_user_profiles_tracked 
                 (login_id, profile_id, date_valid_from)
               VALUES ($1, $2, $3)
               ON CONFLICT (login_id, profile_id, date_valid_from) DO NOTHING",
              params = list(rv$login_id, pid_row$profile_id[1], Sys.Date()))
    
    updateSelectInput(session, "add_standard_profile_select", selected = "")
    profiles_refresh(profiles_refresh() + 1)
    selected_profile_id(pid_row$profile_id[1])
    
    showNotification(
      paste0("\u2713 ", profile_name, " added! Calculating scores in the background..."),
      type = "message", duration = 5
    )
    
    # Insert job into queue
    dbExecute(pool, "
        INSERT INTO dim_job_queue (job_type, login_id, payload)
        VALUES ('score_profile', $1, $2)",
              params = list(rv$login_id, toJSON(list(
                profile_id = pid_row$profile_id[1]
              ), auto_unbox = TRUE)))
    
  }, error = function(e) {
    showNotification(paste("Error adding profile:", e$message), type = "error", duration = 5)
  })
})

# ============================================
# Add custom profile
# ============================================

observeEvent(input$add_custom_profile_btn, {
  req(rv$logged_in, rv$login_id)
  
  pname <- trimws(input$custom_profile_name)
  pdesc <- trimws(input$custom_profile_descriptor)
  
  if (nchar(pname) < 2) {
    showNotification("Please enter a profile name.", type = "error", duration = 3)
    return()
  }
  if (nchar(pdesc) < 10) {
    showNotification("Please enter a profile description.", type = "error", duration = 3)
    return()
  }
  if (user_profile_count() >= MAX_PROFILES) {
    showNotification("Profile limit reached.", type = "error", duration = 3)
    return()
  }
  
  tryCatch({
    new_profile <- dbGetQuery(pool,
                              "INSERT INTO dim_customer_profile 
                                 (profile_name, profile_descriptor, is_standard, 
                                  created_by_login, date_created)
                               VALUES ($1, $2, FALSE, $3, $4) 
                               RETURNING profile_id",
                              params = list(pname, pdesc, rv$login_id, Sys.Date()))
    
    pid <- new_profile$profile_id[1]
    
    dbExecute(pool,
              "INSERT INTO fact_user_profiles_tracked 
                 (login_id, profile_id, date_valid_from)
               VALUES ($1, $2, $3)
               ON CONFLICT (login_id, profile_id, date_valid_from) DO NOTHING",
              params = list(rv$login_id, pid, Sys.Date()))
    
    updateTextInput(session, "custom_profile_name", value = "")
    shinyjs::runjs("document.getElementById('custom_profile_descriptor').value = ''")
    profiles_refresh(profiles_refresh() + 1)
    selected_profile_id(pid)
    
    showNotification(
      paste0("\u2713 '", pname, "' created! Calculating scores in the background..."),
      type = "message", duration = 5
    )
    
    # Insert job into queue
    dbExecute(pool, "
      INSERT INTO dim_job_queue (job_type, login_id, payload)
      VALUES ('score_profile', $1, $2)",
              params = list(rv$login_id, toJSON(list(
                profile_id = pid
              ), auto_unbox = TRUE)))
    
  }, error = function(e) {
    showNotification(paste("Error creating profile:", e$message), type = "error", duration = 5)
  })
})

# ============================================
# Remove profile
# FIX: use date_valid_to = CURRENT_DATE - 1 so the record is 
# immediately excluded by the > CURRENT_DATE filter in user_profiles()
# ============================================

observeEvent(input$remove_profile_id, {
  req(rv$logged_in, rv$login_id)
  pid <- as.integer(input$remove_profile_id)
  
  tryCatch({
    rows_affected <- dbExecute(pool,
                               "UPDATE fact_user_profiles_tracked
               SET date_valid_to = CURRENT_DATE - INTERVAL '1 day'
               WHERE login_id = $1 
                 AND profile_id = $2
                 AND (date_valid_to IS NULL OR date_valid_to > CURRENT_DATE)",
                               params = list(rv$login_id, pid))
    
    if (rows_affected == 0) {
      showNotification("Could not find that persona to remove.", type = "warning", duration = 3)
      return()
    }
    
    # Clear selected state in all contexts
    if (!is.null(selected_profile_id()) && selected_profile_id() == pid) {
      selected_profile_id(NULL)
    }
    if (!is.null(selected_brand_profile_id()) && selected_brand_profile_id() == pid) {
      selected_brand_profile_id(NULL)
    }
    if (!is.null(selected_prompt_profile_id()) && selected_prompt_profile_id() == pid) {
      selected_prompt_profile_id(NULL)
    }
    
    profiles_refresh(profiles_refresh() + 1)
    showNotification("\u2713 Persona removed", type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error removing persona:", e$message), type = "error", duration = 5)
  })
})

# ============================================
# Profile scores display (profiles tab)
# ============================================

output$profile_scores_ui <- renderUI({
  pid <- selected_profile_id()
  
  if (is.null(pid)) {
    return(div(
      style = "text-align: center; padding: 60px; color: #a0aec0;",
      icon("hand-pointer", class = "fa-3x",
           style = "margin-bottom: 15px; color: #e2e8f0;"),
      h4(style = "color: #a0aec0; font-weight: 500;",
         "Select a profile to view scores")
    ))
  }
  
  profiles <- user_profiles()
  prof_row <- profiles[profiles$profile_id == pid, ]
  if (nrow(prof_row) == 0) return(NULL)
  
  prof_name <- prof_row$profile_name[1]
  prof_desc <- prof_row$profile_descriptor[1]
  
  div(
    # Profile header
    div(
      class = "box",
      style = "border-radius: 12px; padding: 20px; margin-bottom: 16px;
               background: linear-gradient(135deg, #1A1A1A 0%, #2D2D2D 100%);
               border-bottom: 3px solid #8E44AD;",
      div(
        style = "display: flex; align-items: flex-start; gap: 16px;",
        div(
          style = "flex: 0 0 48px; width: 48px; height: 48px; border-radius: 12px;
                   background: rgba(142,68,173,0.3); display: flex; align-items: center;
                   justify-content: center; font-size: 20px; color: #C39BD3;",
          icon("users")
        ),
        div(
          style = "flex: 1;",
          div(style = "font-size: 18px; font-weight: 700; color: #F5F5F0; 
                       margin-bottom: 6px;",
              prof_name),
          div(style = "font-size: 12px; color: #9E9E9E; line-height: 1.5; 
                       font-style: italic;",
              paste0('"', prof_desc, '"'))
        )
      )
    ),
    
    # Score cards
    uiOutput(paste0("profile_score_cards_", pid)),
    
    br(),
    
    # Charts
    div(
      class = "box",
      style = "border-radius: 12px; padding: 20px;
               box-shadow: 0 2px 12px rgba(0,0,0,0.06);",
      h4(style = "margin: 0 0 16px; font-weight: 600; color: #2d3748;",
         "Performance Over Time"),
      tabsetPanel(
        type = "tabs",
        tabPanel("AIRR Score",
                 withSpinner(
                   plotlyOutput(paste0("profile_chart_airr_", pid), height = "350px"),
                   type = 4, color = "#8E44AD")),
        tabPanel("Presence",
                 withSpinner(
                   plotlyOutput(paste0("profile_chart_presence_", pid), height = "350px"),
                   type = 4, color = "#8E44AD")),
        tabPanel("Perception",
                 withSpinner(
                   plotlyOutput(paste0("profile_chart_perception_", pid), height = "350px"),
                   type = 4, color = "#8E44AD")),
        tabPanel("Prestige",
                 withSpinner(
                   plotlyOutput(paste0("profile_chart_prestige_", pid), height = "350px"),
                   type = 4, color = "#8E44AD")),
        tabPanel("Persistence",
                 withSpinner(
                   plotlyOutput(paste0("profile_chart_persistence_", pid), height = "350px"),
                   type = 4, color = "#8E44AD"))
      )
    )
  )
})

# ============================================
# Render profile score cards + charts (profiles tab)
# ============================================

observe({
  req(rv$logged_in, rv$login_id)
  profiles <- user_profiles()
  if (nrow(profiles) == 0) return()
  
  lapply(1:nrow(profiles), function(i) {
    pid       <- profiles$profile_id[i]
    local_pid <- pid
    
    # Score cards
    output[[paste0("profile_score_cards_", local_pid)]] <- renderUI({
      scores <- get_profile_brand_scores(rv$login_id, local_pid)
      
      if (is.null(scores) || nrow(scores) == 0) {
        return(div(
          style = "text-align: center; padding: 30px;",
          tags$i(class = "fa fa-spinner fa-spin",
                 style = "font-size: 24px; color: #8E44AD;"),
          p(style = "color: #a0aec0; margin-top: 10px; font-size: 13px;",
            "Calculating persona scores...")
        ))
      }
      
      main <- scores %>% filter(main_brand_flag == TRUE)
      
      airr_val        <- if (nrow(main) > 0) round(main$airr_score[1], 1)        else "—"
      presence_val    <- if (nrow(main) > 0) round(main$presence_score[1], 1)    else "—"
      perception_val  <- if (nrow(main) > 0) round(main$perception_score[1], 1)  else "—"
      prestige_val    <- if (nrow(main) > 0) round(main$prestige_score[1], 1)    else "—"
      persistence_val <- if (nrow(main) > 0) round(main$persistence_score[1], 1) else "—"
      
      fluidRow(
        column(3,
               div(class = "score-card-main",
                   style = "border-bottom-color: #8E44AD; min-height: 120px;",
                   div(class = "score-label", "AiRR Score"),
                   div(class = "score-value", style = "color: #8E44AD;", airr_val))
        ),
        column(9,
               div(class = "score-card-grid",
                   div(class = "score-card-mini presence",
                       div(class = "score-label", "Presence"),
                       div(class = "score-value", presence_val)),
                   div(class = "score-card-mini perception",
                       div(class = "score-label", "Perception"),
                       div(class = "score-value", perception_val)),
                   div(class = "score-card-mini prestige",
                       div(class = "score-label", "Prestige"),
                       div(class = "score-value", prestige_val)),
                   div(class = "score-card-mini persistence",
                       div(class = "score-label", "Persistence"),
                       div(class = "score-value", persistence_val))
               )
        )
      )
    })
    
    # Charts
    chart_specs <- list(
      list(id = "airr",        col = "airr_score",        label = "AIRR Score"),
      list(id = "presence",    col = "presence_score",    label = "Presence Score"),
      list(id = "perception",  col = "perception_score",  label = "Perception Score"),
      list(id = "prestige",    col = "prestige_score",    label = "Prestige Score"),
      list(id = "persistence", col = "persistence_score", label = "Persistence Score")
    )
    
    lapply(chart_specs, function(spec) {
      output[[paste0("profile_chart_", spec$id, "_", local_pid)]] <- renderPlotly({
        ts <- get_profile_brand_timeseries(rv$login_id, local_pid)
        if (is.null(ts) || nrow(ts) == 0) {
          return(plotly_empty_state("No data yet"))
        }
        create_profile_dash_chart(ts, spec$col, spec$label, rv$brand_name)
      })
    })
  })
})

# ============================================
# Brand overview — profile cards section
# ============================================

output$brand_overview_profiles_section <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  sub <- user_subscription()
  
  if (sub$subscription_name != "Enterprise") {
    return(
      div(
        style = "text-align: center; padding: 30px;",
        div(
          style = "background: linear-gradient(135deg, rgba(142,68,173,0.06), 
                   rgba(142,68,173,0.02)); border: 1px dashed rgba(142,68,173,0.3);
                   border-radius: 12px; padding: 30px 20px; max-width: 500px; 
                   margin: 0 auto;",
          icon("crown", class = "fa-2x", style = "color: #8E44AD; margin-bottom: 12px;"),
          div(style = "font-size: 15px; font-weight: 600; color: #2d3748; margin-bottom: 6px;",
              "Customer personas — Enterprise Feature"),
          div(style = "font-size: 13px; color: #718096; margin-bottom: 16px; line-height: 1.5;",
              "See how different customer personas perceive your brand vs competitors."),
          actionButton("upgrade_from_brand_profiles", "View Plans",
                       icon = icon("arrow-up"),
                       style = "background: #8E44AD; color: white; border: none;
                                border-radius: 8px; padding: 8px 20px; font-weight: 600;")
        )
      )
    )
  }
  
  profiles <- user_profiles()
  
  if (nrow(profiles) == 0) {
    return(
      div(
        style = "text-align: center; padding: 30px; color: #a0aec0;",
        icon("users", class = "fa-2x", style = "margin-bottom: 10px; color: #e2e8f0;"),
        p("No personas set up yet."),
        actionButton("manage_profiles_from_brand2", "Add Profiles",
                     icon = icon("plus"),
                     style = "background: #8E44AD; color: white; border: none;
                              border-radius: 8px; padding: 8px 20px; font-weight: 600;")
      )
    )
  }
  
  div(
    style = "display: flex; flex-wrap: wrap; gap: 16px;",
    lapply(1:nrow(profiles), function(i) {
      pid   <- profiles$profile_id[i]
      pname <- profiles$profile_name[i]
      
      is_selected <- !is.null(selected_brand_profile_id()) && 
        selected_brand_profile_id() == pid
      
      div(
        style = paste0(
          "flex: 1; min-width: 200px; cursor: pointer; border-radius: 12px; ",
          "transition: all 0.2s ease; ",
          if (is_selected) {
            "outline: 3px solid #8E44AD; outline-offset: 2px;"
          } else {
            ""
          }
        ),
        onclick = sprintf(
          "Shiny.setInputValue('brand_profile_card_click', %d, {priority: 'event'})",
          pid
        ),
        uiOutput(paste0("brand_overview_profile_card_", pid))
      )
    })
  )
})

# ============================================
# Brand overview — individual profile cards
# ============================================

observe({
  req(rv$logged_in, rv$login_id)
  profiles <- tryCatch(user_profiles(), error = function(e) NULL)
  if (is.null(profiles) || nrow(profiles) == 0) return()
  
  lapply(1:nrow(profiles), function(i) {
    pid        <- profiles$profile_id[i]
    pname      <- profiles$profile_name[i]
    local_pid  <- pid
    local_name <- pname
    
    output[[paste0("brand_overview_profile_card_", local_pid)]] <- renderUI({
      scores <- get_profile_brand_scores(rv$login_id, local_pid)
      
      score_color <- function(val) {
        if (is.na(val) || is.null(val)) return("#cbd5e0")
        if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
      }
      
      div(
        style = "background: white; border-radius: 12px; padding: 16px;
                 border: 1px solid #e2e8f0; border-top: 3px solid #8E44AD;
                 transition: all 0.2s ease;",
        
        # Profile header
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 14px;",
          div(
            style = "width: 28px; height: 28px; border-radius: 8px; 
                     background: rgba(142,68,173,0.1); display: flex; 
                     align-items: center; justify-content: center; color: #8E44AD;",
            icon("users", style = "font-size: 12px;")
          ),
          div(
            style = "font-size: 13px; font-weight: 600; color: #2d3748; 
                     white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
            local_name
          ),
          div(
            style = "margin-left: auto; font-size: 10px; color: #a0aec0;",
            icon("chevron-down", style = "font-size: 10px;")
          )
        ),
        
        if (is.null(scores) || nrow(scores) == 0) {
          div(
            style = "text-align: center; padding: 15px 0; color: #a0aec0;",
            tags$i(class = "fa fa-spinner fa-spin",
                   style = "font-size: 16px; color: #8E44AD;"),
            p(style = "font-size: 11px; margin-top: 6px;", "Calculating...")
          )
        } else {
          main  <- scores %>% filter(main_brand_flag == TRUE)
          comps <- scores %>% filter(main_brand_flag == FALSE) %>%
            arrange(desc(airr_score))
          
          tagList(
            if (nrow(main) > 0) {
              div(
                style = "margin-bottom: 10px;",
                div(
                  style = "display: flex; justify-content: space-between; 
                           align-items: center; margin-bottom: 4px;",
                  tags$span(
                    style = "font-size: 12px; font-weight: 700; color: #2d3748;",
                    paste0("★ ", main$brand_name[1])
                  ),
                  tags$span(
                    style = paste0("font-size: 18px; font-weight: 800; color: ",
                                   score_color(main$airr_score[1]), ";"),
                    round(main$airr_score[1], 1)
                  )
                ),
                div(
                  style = "display: flex; gap: 4px;",
                  lapply(list(
                    list(v = main$presence_score[1],    l = "Pres"),
                    list(v = main$perception_score[1],  l = "Perc"),
                    list(v = main$prestige_score[1],    l = "Prest"),
                    list(v = main$persistence_score[1], l = "Pers")
                  ), function(s) {
                    div(
                      style = "flex: 1; text-align: center;",
                      div(style = paste0("font-size: 11px; font-weight: 700; color: ",
                                         score_color(s$v), ";"),
                          round(s$v, 0)),
                      div(style = "font-size: 9px; color: #a0aec0;", s$l)
                    )
                  })
                )
              )
            },
            
            if (nrow(comps) > 0) hr(style = "margin: 8px 0; border-color: #f0f0f0;"),
            
            if (nrow(comps) > 0) {
              div(
                lapply(1:nrow(comps), function(j) {
                  div(
                    style = "display: flex; justify-content: space-between; 
                             align-items: center; padding: 3px 0;",
                    tags$span(
                      style = "font-size: 12px; color: #4a5568; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; max-width: 130px;",
                      comps$brand_name[j]
                    ),
                    tags$span(
                      style = paste0("font-size: 14px; font-weight: 700; color: ",
                                     score_color(comps$airr_score[j]), ";"),
                      round(comps$airr_score[j], 1)
                    )
                  )
                })
              )
            }
          )
        }
      )
    })
  })
})

# ============================================
# Prompt overview — profile cards section
# ============================================

output$prompt_overview_profiles_section <- renderUI({
  req(rv$logged_in, rv$login_id)
  
  sub <- user_subscription()
  
  if (sub$subscription_name != "Enterprise") {
    return(
      div(
        style = "text-align: center; padding: 30px;",
        div(
          style = "background: linear-gradient(135deg, rgba(142,68,173,0.06), 
                   rgba(142,68,173,0.02)); border: 1px dashed rgba(142,68,173,0.3);
                   border-radius: 12px; padding: 30px 20px; max-width: 500px; 
                   margin: 0 auto;",
          icon("crown", class = "fa-2x", style = "color: #8E44AD; margin-bottom: 12px;"),
          div(style = "font-size: 15px; font-weight: 600; color: #2d3748; margin-bottom: 6px;",
              "Customer personas — Enterprise Feature"),
          div(style = "font-size: 13px; color: #718096; margin-bottom: 16px; line-height: 1.5;",
              "See how different customer segments respond to your tracked prompts."),
          actionButton("upgrade_from_prompt_profiles", "View Plans",
                       icon = icon("arrow-up"),
                       style = "background: #8E44AD; color: white; border: none;
                                border-radius: 8px; padding: 8px 20px; font-weight: 600;")
        )
      )
    )
  }
  
  profiles <- user_profiles()
  
  if (nrow(profiles) == 0) {
    return(
      div(
        style = "text-align: center; padding: 30px; color: #a0aec0;",
        icon("users", class = "fa-2x", style = "margin-bottom: 10px; color: #e2e8f0;"),
        p("No personas set up yet."),
        actionButton("manage_profiles_from_prompt2", "Add personas",
                     icon = icon("plus"),
                     style = "background: #8E44AD; color: white; border: none;
                              border-radius: 8px; padding: 8px 20px; font-weight: 600;")
      )
    )
  }
  
  req(input$dash_query_select, input$dash_query_select != "")
  
  div(
    style = "display: flex; flex-wrap: wrap; gap: 16px;",
    lapply(1:nrow(profiles), function(i) {
      pid   <- profiles$profile_id[i]
      pname <- profiles$profile_name[i]
      
      is_selected <- !is.null(selected_prompt_profile_id()) && 
        selected_prompt_profile_id() == pid
      
      div(
        style = paste0(
          "flex: 1; min-width: 200px; cursor: pointer; border-radius: 12px; ",
          "transition: all 0.2s ease; ",
          if (is_selected) {
            "outline: 3px solid #8E44AD; outline-offset: 2px;"
          } else {
            ""
          }
        ),
        onclick = sprintf(
          "Shiny.setInputValue('prompt_profile_card_click', %d, {priority: 'event'})",
          pid
        ),
        uiOutput(paste0("prompt_overview_profile_card_", pid))
      )
    })
  )
})

# ============================================
# Prompt overview — individual profile cards
# ============================================

observe({
  req(rv$logged_in, rv$login_id)
  req(input$dash_query_select, input$dash_query_select != "")
  
  profiles <- tryCatch(user_profiles(), error = function(e) NULL)
  if (is.null(profiles) || nrow(profiles) == 0) return()
  
  query_string <- input$dash_query_select
  
  lapply(1:nrow(profiles), function(i) {
    pid        <- profiles$profile_id[i]
    pname      <- profiles$profile_name[i]
    local_pid  <- pid
    local_name <- pname
    
    output[[paste0("prompt_overview_profile_card_", local_pid)]] <- renderUI({
      scores <- get_profile_query_scores(rv$login_id, local_pid, query_string)
      
      score_color <- function(val) {
        if (is.na(val) || is.null(val)) return("#cbd5e0")
        if (val >= 70) "#48bb78" else if (val >= 40) "#ecc94b" else "#fc8181"
      }
      
      div(
        style = "background: white; border-radius: 12px; padding: 16px;
                 border: 1px solid #e2e8f0; border-top: 3px solid #8E44AD;
                 transition: all 0.2s ease;",
        
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 14px;",
          div(
            style = "width: 28px; height: 28px; border-radius: 8px; 
                     background: rgba(142,68,173,0.1); display: flex; 
                     align-items: center; justify-content: center; color: #8E44AD;",
            icon("users", style = "font-size: 12px;")
          ),
          div(
            style = "font-size: 13px; font-weight: 600; color: #2d3748;
                     white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
            local_name
          ),
          div(
            style = "margin-left: auto; font-size: 10px; color: #a0aec0;",
            icon("chevron-down", style = "font-size: 10px;")
          )
        ),
        
        if (is.null(scores) || nrow(scores) == 0) {
          div(
            style = "text-align: center; padding: 15px 0; color: #a0aec0;",
            tags$i(class = "fa fa-spinner fa-spin",
                   style = "font-size: 16px; color: #8E44AD;"),
            p(style = "font-size: 11px; margin-top: 6px;", "Calculating...")
          )
        } else {
          main  <- scores %>% filter(main_brand_flag == TRUE)
          comps <- scores %>% filter(main_brand_flag == FALSE) %>%
            arrange(desc(airr_score))
          
          tagList(
            if (nrow(main) > 0) {
              div(
                style = "margin-bottom: 10px;",
                div(
                  style = "display: flex; justify-content: space-between; 
                           align-items: center; margin-bottom: 4px;",
                  tags$span(
                    style = "font-size: 12px; font-weight: 700; color: #2d3748;",
                    paste0("★ ", main$brand_name[1])
                  ),
                  tags$span(
                    style = paste0("font-size: 18px; font-weight: 800; color: ",
                                   score_color(main$airr_score[1]), ";"),
                    round(main$airr_score[1], 1)
                  )
                ),
                div(
                  style = "display: flex; gap: 4px;",
                  lapply(list(
                    list(v = main$presence_score[1],    l = "Pres"),
                    list(v = main$perception_score[1],  l = "Perc"),
                    list(v = main$prestige_score[1],    l = "Prest"),
                    list(v = main$persistence_score[1], l = "Pers")
                  ), function(s) {
                    div(
                      style = "flex: 1; text-align: center;",
                      div(style = paste0("font-size: 11px; font-weight: 700; color: ",
                                         score_color(s$v), ";"),
                          round(s$v, 0)),
                      div(style = "font-size: 9px; color: #a0aec0;", s$l)
                    )
                  })
                )
              )
            },
            
            if (nrow(comps) > 0) hr(style = "margin: 8px 0; border-color: #f0f0f0;"),
            
            if (nrow(comps) > 0) {
              div(
                lapply(1:nrow(comps), function(j) {
                  div(
                    style = "display: flex; justify-content: space-between; 
                             align-items: center; padding: 3px 0;",
                    tags$span(
                      style = "font-size: 12px; color: #4a5568; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; max-width: 130px;",
                      comps$brand_name[j]
                    ),
                    tags$span(
                      style = paste0("font-size: 14px; font-weight: 700; color: ",
                                     score_color(comps$airr_score[j]), ";"),
                      round(comps$airr_score[j], 1)
                    )
                  )
                })
              )
            }
          )
        }
      )
    })
  })
})

# ============================================
# Navigation helpers
# ============================================

observeEvent(input$upgrade_from_brand_profiles, {
  shinyjs::click("upgrade_btn")
})

observeEvent(input$upgrade_from_prompt_profiles, {
  shinyjs::click("upgrade_btn")
})

observeEvent(input$manage_profiles_from_brand2, {
  updateTabItems(session, "sidebar", "profiles")
})

observeEvent(input$manage_profiles_from_prompt2, {
  updateTabItems(session, "sidebar", "profiles")
})

# ============================================
# DB helpers — brand profile scores
# ============================================

get_profile_brand_scores <- function(login_id, profile_id) {
  brands <- dbGetQuery(pool, "
    SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
  ", params = list(login_id))
  
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids    <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  scores <- dbGetQuery(pool, sprintf("
    WITH latest AS (
      SELECT brand_id, MAX(date) as latest_date
      FROM fact_profile_brand_history
      WHERE profile_id = $1 AND brand_id IN (%s)
      GROUP BY brand_id
    )
    SELECT fpbh.brand_id, fpbh.airr_score, fpbh.presence_score,
           fpbh.perception_score, fpbh.prestige_score, 
           fpbh.persistence_score, fpbh.date
    FROM fact_profile_brand_history fpbh
    INNER JOIN latest l 
      ON fpbh.brand_id = l.brand_id AND fpbh.date = l.latest_date
    WHERE fpbh.profile_id = $1
  ", placeholders), params = as.list(c(profile_id, brand_ids)))
  
  if (nrow(scores) == 0) return(NULL)
  
  scores %>%
    left_join(brands %>% select(brand_id, brand_name, main_brand_flag),
              by = "brand_id")
}

get_profile_brand_timeseries <- function(login_id, profile_id) {
  brands <- dbGetQuery(pool, "
    SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
  ", params = list(login_id))
  
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids    <- brands$brand_id
  placeholders <- paste0("$", 2:(length(brand_ids) + 1), collapse = ", ")
  
  ts <- dbGetQuery(pool, sprintf("
    SELECT brand_id, date, airr_score, presence_score,
           perception_score, prestige_score, persistence_score
    FROM fact_profile_brand_history
    WHERE profile_id = $1 AND brand_id IN (%s)
    ORDER BY brand_id, date
  ", placeholders), params = as.list(c(profile_id, brand_ids)))
  
  if (nrow(ts) == 0) return(NULL)
  
  ts %>%
    left_join(brands %>% select(brand_id, brand_name, main_brand_flag),
              by = "brand_id")
}

# ============================================
# DB helpers — prompt profile scores
# ============================================

get_profile_query_scores <- function(login_id, profile_id, query_string) {
  brands <- dbGetQuery(pool, "
    SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
  ", params = list(login_id))
  
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids    <- brands$brand_id
  placeholders <- paste0("$", 3:(length(brand_ids) + 2), collapse = ", ")
  
  scores <- dbGetQuery(pool, sprintf("
    WITH latest AS (
      SELECT fpqh.brand_id, MAX(fpqh.date) as latest_date
      FROM fact_profile_query_history fpqh
      JOIN dim_query dq ON dq.query_id = fpqh.query_id
      WHERE fpqh.profile_id = $1
        AND dq.query_string = $2
        AND fpqh.brand_id IN (%s)
      GROUP BY fpqh.brand_id
    )
    SELECT fpqh.brand_id, fpqh.airr_score, fpqh.presence_score,
           fpqh.perception_score, fpqh.prestige_score,
           fpqh.persistence_score, fpqh.date
    FROM fact_profile_query_history fpqh
    JOIN dim_query dq ON dq.query_id = fpqh.query_id
    INNER JOIN latest l
      ON fpqh.brand_id = l.brand_id AND fpqh.date = l.latest_date
    WHERE fpqh.profile_id = $1
      AND dq.query_string = $2
  ", placeholders), params = as.list(c(profile_id, query_string, brand_ids)))
  
  if (nrow(scores) == 0) return(NULL)
  
  scores %>%
    left_join(brands %>% select(brand_id, brand_name, main_brand_flag),
              by = "brand_id")
}

get_profile_query_timeseries <- function(login_id, profile_id, query_string) {
  brands <- dbGetQuery(pool, "
    SELECT b.brand_id, b.brand_name, ubt.main_brand_flag
    FROM fact_user_brands_tracked ubt
    JOIN dim_brand b ON b.brand_id = ubt.brand_id
    WHERE ubt.login_id = $1
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to > CURRENT_DATE)
  ", params = list(login_id))
  
  if (nrow(brands) == 0) return(NULL)
  
  brand_ids    <- brands$brand_id
  placeholders <- paste0("$", 3:(length(brand_ids) + 2), collapse = ", ")
  
  ts <- dbGetQuery(pool, sprintf("
    SELECT fpqh.brand_id, fpqh.date, fpqh.airr_score, fpqh.presence_score,
           fpqh.perception_score, fpqh.prestige_score, fpqh.persistence_score
    FROM fact_profile_query_history fpqh
    JOIN dim_query dq ON dq.query_id = fpqh.query_id
    WHERE fpqh.profile_id = $1
      AND dq.query_string = $2
      AND fpqh.brand_id IN (%s)
    ORDER BY fpqh.brand_id, fpqh.date
  ", placeholders), params = as.list(c(profile_id, query_string, brand_ids)))
  
  if (nrow(ts) == 0) return(NULL)
  
  ts %>%
    left_join(brands %>% select(brand_id, brand_name, main_brand_flag),
              by = "brand_id")
}

# ============================================
# Chart helpers
# ============================================

create_profile_dash_chart <- function(data, score_col, y_title, main_brand_name) {
  
  main_data <- data %>% filter(main_brand_flag == TRUE)
  comp_data <- data %>% filter(main_brand_flag == FALSE)
  
  comp_colours <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12",
                    "#E67E22", "#1ABC9C", "#34495E", "#16A085",
                    "#C0392B", "#2980B9")
  
  p <- plot_ly()
  
  if (nrow(comp_data) > 0) {
    comp_brands <- unique(comp_data$brand_name)
    for (idx in seq_along(comp_brands)) {
      bn  <- comp_brands[idx]
      bd  <- comp_data %>% filter(brand_name == bn)
      col <- comp_colours[((idx - 1) %% length(comp_colours)) + 1]
      
      p <- p %>% add_trace(
        data = bd, x = ~date, y = as.formula(paste0("~", score_col)),
        type = 'scatter', mode = 'lines+markers',
        name = bn,
        line   = list(width = 2, dash = "dot", shape = "spline", color = col),
        marker = list(size = 4, color = col),
        opacity = 0.7,
        hovertemplate = paste0('<b>', bn, '</b><br>%{y:.1f}<extra></extra>')
      )
    }
  }
  
  if (nrow(main_data) > 0) {
    p <- p %>% add_trace(
      data = main_data, x = ~date, y = as.formula(paste0("~", score_col)),
      type = 'scatter', mode = 'lines+markers',
      name = paste0("★ ", main_brand_name),
      line   = list(width = 3.5, color = '#8E44AD', shape = "spline"),
      marker = list(size = 7, color = '#8E44AD',
                    line = list(color = '#1A1A1A', width = 1.5)),
      hovertemplate = paste0('<b>', main_brand_name, '</b><br>%{y:.1f}<extra></extra>')
    )
  }
  
  plotly_pro_layout(p, y_title, show_legend = TRUE)
}

render_profile_spider <- function(scores, main_brand_name) {
  
  comp_colours <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12",
                    "#E67E22", "#1ABC9C", "#34495E", "#16A085",
                    "#C0392B", "#2980B9")
  
  p <- plot_ly(type = 'scatterpolar', fill = 'toself')
  
  comp_data <- scores %>% filter(main_brand_flag == FALSE)
  for (i in seq_len(nrow(comp_data))) {
    col <- comp_colours[((i - 1) %% length(comp_colours)) + 1]
    p <- p %>% add_trace(
      r = c(comp_data$presence_score[i],   comp_data$perception_score[i],
            comp_data$prestige_score[i],   comp_data$persistence_score[i],
            comp_data$presence_score[i]),
      theta  = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
      name   = comp_data$brand_name[i],
      fillcolor = paste0(col, '15'),
      line   = list(width = 2, color = col),
      marker = list(size = 5, color = col),
      opacity = 0.7,
      hovertemplate = paste0(
        '<b>', comp_data$brand_name[i], '</b><br>',
        '%{theta}: %{r:.1f}<extra></extra>')
    )
  }
  
  main_data <- scores %>% filter(main_brand_flag == TRUE)
  if (nrow(main_data) > 0) {
    p <- p %>% add_trace(
      r = c(main_data$presence_score[1],   main_data$perception_score[1],
            main_data$prestige_score[1],   main_data$persistence_score[1],
            main_data$presence_score[1]),
      theta  = c('Presence', 'Perception', 'Prestige', 'Persistence', 'Presence'),
      name   = paste0("★ ", main_brand_name),
      fillcolor = 'rgba(142, 68, 173, 0.15)',
      line   = list(width = 3, color = '#8E44AD'),
      marker = list(size = 8, color = '#8E44AD'),
      hovertemplate = paste0(
        '<b>', main_brand_name, '</b><br>',
        '%{theta}: %{r:.1f}<extra></extra>')
    )
  }
  
  p %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE, range = c(0, 100),
        showline = FALSE, showticklabels = TRUE,
        gridcolor = "rgba(0,0,0,0.06)",
        tickfont = list(size = 10, color = "#9E9E9E", family = "Inter")
      ),
      angularaxis = list(
        showline = FALSE,
        gridcolor = "rgba(0,0,0,0.06)",
        tickfont = list(size = 12, color = "#4a5568", family = "Inter")
      ),
      bgcolor = "rgba(0,0,0,0)"
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h", yanchor = "top", y = -0.15,
      xanchor = "center", x = 0.5,
      font = list(size = 11, color = "#9E9E9E", family = "Inter"),
      bgcolor = "rgba(0,0,0,0)"
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 60, r = 60, t = 30, b = 50),
    font = list(family = "Inter")
  ) %>% config(displayModeBar = FALSE)
}

plotly_empty_state <- function(message = "No data available") {
  plot_ly() %>%
    layout(
      annotations = list(
        text = message, xref = "paper", yref = "paper",
        x = 0.5, y = 0.5, showarrow = FALSE,
        font = list(size = 14, color = "#a0aec0")
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    ) %>%
    config(displayModeBar = FALSE)
}