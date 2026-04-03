tab_account <- tabItem(
  tabName = "account",
  
  fluidRow(
    column(width = 3, uiOutput("account_profile_card")),
    column(width = 2, uiOutput("account_brand_gauge")),
    column(width = 2, uiOutput("account_query_gauge")),
    column(width = 2, uiOutput("account_persona_gauge")),
    column(width = 3, uiOutput("account_upgrade_card"))
  ),
  
  br(),
  
  # Industry nudge
  uiOutput("account_industry_nudge"),
  
  fluidRow(
    box(
      title = NULL, width = 6,
      div(
        style = "display: flex; justify-content: space-between;
                 align-items: center; margin-bottom: 15px;",
        h4(style = "margin: 0; font-weight: 600; color: #2d3748;",
           "Competitor Brands"),
        uiOutput("account_brand_slot_badge")
      ),
      fluidRow(
        column(8,
               textInput("add_competitor_brand_input", NULL,
                         placeholder = "Enter a competitor brand name...",
                         width = "100%")),
        column(4,
               tags$div(style = "margin-top: 0px;",
                        actionButton("add_competitor_btn", "Add",
                                     icon = icon("plus"),
                                     class = "btn-primary",
                                     width = "100%")))
      ),
      uiOutput("account_competitor_timing_notice"),
      hr(style = "margin: 12px 0; border-color: #f0f0f0;"),
      uiOutput("account_competitor_list")
    ),
    
    box(
      title = NULL, width = 6,
      div(
        style = "display: flex; justify-content: space-between;
                 align-items: center; margin-bottom: 15px;",
        h4(style = "margin: 0; font-weight: 600; color: #2d3748;",
           "Tracked Prompts"),
        uiOutput("account_query_slot_badge")
      ),
      fluidRow(
        column(8,
               textInput("add_query_input", NULL,
                         placeholder = "Enter a prompt to track...",
                         width = "100%")),
        column(4,
               tags$div(style = "margin-top: 0px;",
                        actionButton("add_query_btn", "Add",
                                     icon = icon("plus"),
                                     class = "btn-primary",
                                     width = "100%")))
      ),
      uiOutput("account_prompt_timing_notice"),
      hr(style = "margin: 12px 0; border-color: #f0f0f0;"),
      uiOutput("account_query_list")
    )
  ),
  fluidRow(
    box(
      title = NULL, width = 12,
      div(
        style = "display: flex; justify-content: space-between;
               align-items: center; margin-bottom: 15px;",
        div(
          h4(style = "margin: 0; font-weight: 600; color: #2d3748;",
             "Brand Aliases"),
          p(style = "margin: 4px 0 0; font-size: 12px; color: #a0aec0;",
            "Alternative names your brand is known by — used when scanning
           AI responses for mentions.")
        )
      ),
      uiOutput("account_alias_section")
    )
  )
)