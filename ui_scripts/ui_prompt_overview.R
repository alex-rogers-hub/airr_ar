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
          
          # Label
          div(
            style = "flex: 0 0 auto;",
            tags$span(
              style = "color: #a0aec0; font-size: 11px; text-transform: uppercase; 
                       letter-spacing: 1.5px; font-weight: 600;",
              icon("comment-dots", style = "margin-right: 6px;"),
              "Tracking Prompt"
            )
          ),
          
          # Selector
          div(
            style = "flex: 1; min-width: 300px;",
            class = "prompt-strip-select",
            selectInput("dash_query_select", NULL,
                        choices = NULL, selected = NULL, width = "100%")
          ),
          
          # Buttons
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
              "Export",
              class = "btn-download",
              style = "border-radius: 8px; padding: 8px 16px;"
            )
          )
        )
      )
    )
  ),
  
  # Performance Overview: Trend + Spider
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
          class = "chart-row-flex",
          div(
            class = "chart-col-trend",
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
          div(
            class = "chart-col-spider",
            h5(style = "font-weight: 600; color: #4a5568; margin-bottom: 10px; 
                       text-align: center;",
               "Prompt Comparison"),
            withSpinner(
              plotlyOutput("dash_query_spider", height = "400px"),
              type = 4, color = "#667eea"
            )
          )
        )
      )
    )
  ),
  
  br(),
  
  # Prompt Rankings Table
  fluidRow(
    column(
      width = 12,
      div(
        class = "box",
        style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); padding: 20px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; 
                   margin-bottom: 15px;",
          h4(style = "margin: 0; font-weight: 600; color: #2d3748;", "Prompt Rankings"),
          downloadButton("download_prompt_rankings", "Export",
                         class = "btn-download")
        ),
        withSpinner(
          uiOutput("dash_query_rankings_table"),
          type = 4, color = "#667eea"
        )
      )
    )
  ),
  br(),
  
  # AI Analysis
  fluidRow(
    column(
      width = 12,
      div(
        class = "box",
        style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); padding: 20px;
                 border-top: 3px solid #667eea;",
        div(
          style = "margin-bottom: 15px;",
          h4(style = "margin: 0; font-weight: 600; color: #2d3748;", 
             icon("wand-magic-sparkles", style = "color: #667eea; margin-right: 8px;"),
             "AI-Powered Analysis")
        ),
        uiOutput("prompt_ai_summary_ui")
      )
    )
  )
)