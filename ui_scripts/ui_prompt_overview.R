tab_prompt_overview <- tabItem(
  tabName = "prompt_overview",
  
  # Prompt selector strip
  fluidRow(
    column(
      width = 12,
      div(
        style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); 
                 padding: 16px 24px; margin-bottom: 20px;
                 background: linear-gradient(135deg, #1a202c 0%, #2d3748 100%);",
        div(
          style = "display: flex; align-items: center; gap: 16px; flex-wrap: wrap;",
          div(
            style = "flex: 0 0 auto;",
            tags$span(
              style = "color: #a0aec0; font-size: 11px; text-transform: uppercase; 
                       letter-spacing: 1.5px; font-weight: 600;",
              icon("comment-dots", style = "margin-right: 6px;"),
              "Tracking Prompt"
            )
          ),
          div(
            style = "flex: 1; min-width: 300px;",
            class = "prompt-strip-select",
            selectInput("dash_query_select", NULL,
                        choices = NULL, selected = NULL, width = "100%")
          ),
          div(
            style = "flex: 0 0 auto; display: flex; gap: 8px;",
            actionButton(
              "add_prompt_from_overview",
              "Add Prompt",
              icon = icon("plus"),
              style = "background: #667eea; color: white; border: none; border-radius: 8px; 
                       padding: 8px 16px; font-weight: 600; font-size: 13px;"
            ),
            downloadButton(
              "download_prompt_data",
              "Export CSV",
              class = "btn-download",
              style = "border-radius: 8px; padding: 8px 16px;"
            ),
            tags$span(
              title = "Coming soon",
              style = "cursor: not-allowed; display: inline-block;",
              tags$button(
                "Download Report",
                icon("file-alt"),
                disabled = "disabled",
                style = "background: linear-gradient(135deg, #667eea, #764ba2);
             color: white; border: none; font-weight: 600;
             border-radius: 8px; padding: 8px 16px; font-size: 13px;
             opacity: 0.55; cursor: not-allowed; pointer-events: none;"
              )
            )
            # downloadButton(
            #   "download_prompt_report",
            #   "Download Report",
            #   style = "background: linear-gradient(135deg, #667eea, #764ba2); color: white; 
            #            border: none; border-radius: 8px; padding: 8px 16px; font-weight: 600;
            #            font-size: 13px;"
            # )
          )
        )
      )
    )
  ),
  
  # Performance Overview: Trend chart + Rankings side by side
  fluidRow(
    column(
      width = 12,
      div(
        class = "box",
        style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); padding: 20px;",
        
        div(
          style = "display: flex; justify-content: space-between; align-items: center; 
                   margin-bottom: 15px;",
          h4(style = "margin: 0; font-weight: 600; color: #2d3748;",
             "Performance Overview")
        ),
        
        div(
          style = "display: flex; gap: 20px; align-items: stretch;",
          
          # Left: trend charts
          div(
            style = "flex: 0 0 55%; min-width: 0;",
            tabsetPanel(
              id = "dash_query_chart_tabs",
              type = "tabs",
              tabPanel("AIRR Score",
                       withSpinner(plotlyOutput("dash_query_chart_airr", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Presence",
                       withSpinner(plotlyOutput("dash_query_chart_presence", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Perception",
                       withSpinner(plotlyOutput("dash_query_chart_perception", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Prestige",
                       withSpinner(plotlyOutput("dash_query_chart_prestige", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Persistence",
                       withSpinner(plotlyOutput("dash_query_chart_persistence", height = "420px"),
                                   type = 4, color = "#667eea"))
            )
          ),
          
          # Right: compact rankings — vertically centred
          div(
            style = "flex: 1; min-width: 320px; display: flex; 
           align-items: center;",
            div(
              style = "width: 100%;",
              withSpinner(
                uiOutput("dash_query_rankings_table_compact"),
                type = 4, color = "#667eea"
              )
            )
          )
        )
      )
    )
  ),
  
  br(),
  
  # Spider + Quadrant charts — each a third
  fluidRow(
    column(
      width = 12,
      div(
        class = "box",
        style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); 
                 padding: 20px;",
        h4(style = "margin: 0 0 15px; font-weight: 600; color: #2d3748;",
           "Prompt Positioning"),
        
        div(
          style = "display: flex; gap: 20px;",
          
          # Spider chart — left third
          div(
            style = "flex: 1; min-width: 0; background: rgba(102,126,234,0.03); 
                     border-radius: 10px; padding: 12px; 
                     border: 1px solid rgba(102,126,234,0.1);",
            withSpinner(
              plotlyOutput("dash_query_spider", height = "380px"),
              type = 4, color = "#667eea"
            )
          ),
          
          # Positioning quadrant — middle third
          div(
            style = "flex: 1; min-width: 0; background: rgba(102,126,234,0.03); 
                     border-radius: 10px; padding: 12px; 
                     border: 1px solid rgba(102,126,234,0.1);",
            withSpinner(
              plotlyOutput("dash_query_quadrant_positioning", height = "380px"),
              type = 4, color = "#667eea"
            )
          ),
          
          # Momentum quadrant — right third
          div(
            style = "flex: 1; min-width: 0; background: rgba(102,126,234,0.03); 
                     border-radius: 10px; padding: 12px; 
                     border: 1px solid rgba(102,126,234,0.1);",
            withSpinner(
              plotlyOutput("dash_query_quadrant_momentum", height = "380px"),
              type = 4, color = "#667eea"
            )
          )
        )
      )
    )
  ),
  
  br(),
  
  # Customer Profiles section
  fluidRow(
    column(
      width = 12,
      div(
        class = "box",
        style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); 
               padding: 20px;",
        div(
          style = "display: flex; justify-content: space-between;
           align-items: center; margin-bottom: 15px;",
          h4(style = "margin: 0; font-weight: 600; color: #2d3748;",
             "Customer Persona Scores"),
          actionButton(
            "manage_profiles_from_prompt",
            "Manage Personas",
            icon  = icon("users"),
            style = "background: #8E44AD; color: white; border: none;
             border-radius: 8px; padding: 6px 14px; font-weight: 600;
             font-size: 13px;"
          )
        ),
        
        uiOutput("prompt_overview_profiles_section"),
        uiOutput("prompt_profile_detail_panel")
      )
    )
  )
)