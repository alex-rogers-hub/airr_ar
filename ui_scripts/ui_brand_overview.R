tab_brand_overview <- tabItem(
  tabName = "brand_overview",
  
  # Score Cards
  uiOutput("dash_score_cards_row"),
  br(),
  
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
             "Performance Overview"),
          div(
            style = "display: flex; gap: 10px;",
            actionButton(
              "add_competitor_from_brand",
              "Add Competitor",
              icon = icon("plus"),
              class = "btn-primary btn-sm",
              style = "font-weight: 600;"
            ),
            downloadButton("download_brand_data", "Export Data",
                           class = "btn-download")
          )
        ),
        
        div(
          class = "chart-row-flex",
          div(
            class = "chart-col-trend",
            tabsetPanel(
              id = "dash_chart_tabs",
              type = "tabs",
              tabPanel("AIRR Score",
                       withSpinner(plotlyOutput("dash_chart_airr", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Presence",
                       withSpinner(plotlyOutput("dash_chart_presence", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Perception",
                       withSpinner(plotlyOutput("dash_chart_perception", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Prestige",
                       withSpinner(plotlyOutput("dash_chart_prestige", height = "420px"),
                                   type = 4, color = "#667eea")),
              tabPanel("Persistence",
                       withSpinner(plotlyOutput("dash_chart_persistence", height = "420px"),
                                   type = 4, color = "#667eea"))
            )
          ),
          div(
            class = "chart-col-spider",
            h5(style = "font-weight: 600; color: #4a5568; margin-bottom: 10px; 
                       text-align: center;",
               "Brand Comparison"),
            withSpinner(
              plotlyOutput("dash_spider_compare", height = "400px"),
              type = 4, color = "#667eea"
            )
          )
        )
      )
    )
  ),
  
  br(),
  
  # Rankings Table
  fluidRow(
    column(
      width = 12,
      div(
        class = "box",
        style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); padding: 20px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; 
                   margin-bottom: 15px;",
          h4(style = "margin: 0; font-weight: 600; color: #2d3748;", "Brand Rankings"),
          downloadButton("download_rankings_data", "Export",
                         class = "btn-download")
        ),
        withSpinner(
          uiOutput("dash_rankings_table"),
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
        uiOutput("brand_ai_summary_ui")
      )
    )
  )
)