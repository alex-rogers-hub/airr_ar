tab_brand_overview <- tabItem(
  tabName = "brand_overview",
  
  # Score Cards
  uiOutput("dash_score_cards_row"),
  br(),
  
  # Performance Overview: Trend chart + Compact Rankings side by side
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
             "Overall Brand Performance Overview"),
          div(
            style = "display: flex; gap: 10px;",
            actionButton(
              "add_competitor_from_brand",
              "Add Competitor",
              icon = icon("plus"),
              class = "btn-primary btn-sm",
              style = "font-weight: 600;"
            ),
            downloadButton("download_brand_data", "Export CSV",
                           class = "btn-download"),
            tags$span(
              title = "Coming soon",
              style = "cursor: not-allowed; display: inline-block;",
              tags$button(
                "Download Report",
                icon("file-alt"),
                disabled = "disabled",
                style = "background: linear-gradient(135deg, #667eea, #764ba2); 
             color: white; border: none; font-weight: 600;
             border-radius: 4px; padding: 6px 12px; font-size: 14px;
             opacity: 0.55; cursor: not-allowed; pointer-events: none;"
              )
            )
            # downloadButton("download_brand_report", "Download Report",
            #                class = "btn-download",
            #                style = "background: linear-gradient(135deg, #667eea, #764ba2); 
            #                         color: white; border: none; font-weight: 600;")
          )
        ),
        
        # Trend chart + Rankings side by side
        div(
          style = "display: flex; gap: 20px; align-items: stretch;",
          
          # Left: trend charts
          div(
            style = "flex: 0 0 55%; min-width: 0;",
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
          
          # Right: compact rankings — vertically centred
          div(
            style = "flex: 1; min-width: 320px; display: flex; 
           align-items: center;",
            div(
              style = "width: 100%;",
              withSpinner(
                uiOutput("dash_rankings_table_compact"),
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
           "Overall Brand Positioning"),
        
        div(
          style = "display: flex; gap: 20px;",
          
          # Spider chart — left third
          div(
            style = "flex: 1; min-width: 0; background: rgba(102,126,234,0.03); 
                     border-radius: 10px; padding: 12px; 
                     border: 1px solid rgba(102,126,234,0.1);",
            withSpinner(
              plotlyOutput("dash_spider_compare", height = "380px"),
              type = 4, color = "#667eea"
            )
          ),
          
          # Positioning quadrant — middle third
          div(
            style = "flex: 1; min-width: 0; background: rgba(102,126,234,0.03); 
                     border-radius: 10px; padding: 12px; 
                     border: 1px solid rgba(102,126,234,0.1);",
            withSpinner(
              plotlyOutput("dash_quadrant_positioning", height = "380px"),
              type = 4, color = "#667eea"
            )
          ),
          
          # Momentum quadrant — right third
          div(
            style = "flex: 1; min-width: 0; background: rgba(102,126,234,0.03); 
                     border-radius: 10px; padding: 12px; 
                     border: 1px solid rgba(102,126,234,0.1);",
            withSpinner(
              plotlyOutput("dash_quadrant_momentum", height = "380px"),
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
          div(
            style = "display: flex; align-items: center; gap: 12px;",
            h4(style = "margin: 0; font-weight: 600; color: #2d3748;",
               "Customer persona scores"),
            tags$span(
              style = "background: #8E44AD; color: white; font-size: 10px; 
                     padding: 3px 10px; border-radius: 10px; font-weight: 600;
                     letter-spacing: 0.5px;",
              "ENTERPRISE"
            )
          ),
          actionButton(
            "manage_profiles_from_brand",
            "Manage personas",
            icon = icon("users"),
            style = "background: #8E44AD; color: white; border: none; 
                   border-radius: 8px; padding: 6px 14px; font-weight: 600;
                   font-size: 13px;"
          )
        ),
        
        uiOutput("brand_overview_profiles_section"),
        uiOutput("brand_profile_detail_panel")
      )
    )
  )
)