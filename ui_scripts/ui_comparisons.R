tab_comparisons <- tabItem(
  tabName = "comparisons",
  
  fluidRow(
    box(
      title = "Brand Rankings by AIRR Score",
      status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      DTOutput("leaderboard_table")
    )
  ),
  
  fluidRow(
    box(
      title = "Score Trends Over Time",
      status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      tabsetPanel(
        id = "leaderboard_chart_tabs", type = "tabs",
        tabPanel("AIRR Score", plotlyOutput("leaderboard_chart_airr", height = "500px")),
        tabPanel("Presence Score", plotlyOutput("leaderboard_chart_presence", height = "500px")),
        tabPanel("Perception Score", plotlyOutput("leaderboard_chart_perception", height = "500px")),
        tabPanel("Prestige Score", plotlyOutput("leaderboard_chart_prestige", height = "500px")),
        tabPanel("Persistence Score", plotlyOutput("leaderboard_chart_persistence", height = "500px"))
      )
    )
  ),
  
  fluidRow(
    box(
      title = "Select Brands to Compare",
      status = "primary", solidHeader = TRUE, width = 12,
      fluidRow(
        column(4, selectInput("compare_customer_1", "Brand 1:", choices = NULL, selected = NULL)),
        column(4, selectInput("compare_customer_2", "Brand 2:", choices = NULL, selected = NULL)),
        column(4, selectInput("compare_customer_3", "Brand 3:", choices = NULL, selected = NULL))
      ),
      fluidRow(column(12, plotlyOutput("spider_chart_compare", height = "600px")))
    )
  ),
  
  fluidRow(
    box(
      title = "Add New Prompt for All Brands",
      status = "primary", solidHeader = TRUE, width = 12,
      fluidRow(
        column(9, textInput("new_prompt_input", NULL, placeholder = "Enter your prompt here...", width = "100%")),
        column(3, tags$div(style = "margin-top: 0px;",
                           actionButton("submit_prompt_to_all_btn", "Add Prompt",
                                        icon = icon("plus"), class = "btn-primary", width = "100%")))
      ),
      fluidRow(column(9, selectInput("query_select", "Choose a prompt to analyze:",
                                     choices = NULL, selected = NULL, width = "100%"))),
      fluidRow(column(12, DTOutput("query_top10_table"))),
      fluidRow(
        column(12,
               tabsetPanel(
                 id = "query_chart_tabs", type = "tabs",
                 tabPanel("AIRR Score", plotlyOutput("query_chart_airr", height = "500px")),
                 tabPanel("Presence Score", plotlyOutput("query_chart_presence", height = "500px")),
                 tabPanel("Perception Score", plotlyOutput("query_chart_perception", height = "500px")),
                 tabPanel("Prestige Score", plotlyOutput("query_chart_prestige", height = "500px")),
                 tabPanel("Persistence Score", plotlyOutput("query_chart_persistence", height = "500px"))
               ))
      )
    )
  )
)