source("global.R")
source("custom_css_stuff.R")


ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "AiRR",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      uiOutput("user_info")
    ),
    tags$li(
      class = "dropdown",
      actionButton(
        "logout",
        "Logout",
        icon = icon("sign-out-alt"),
        style = "margin-top: 8px; margin-right: 10px;
                 background: rgba(255,255,255,0.2); 
                 border: none; 
                 color: white;
                 border-radius: 5px;
                 padding: 8px 15px;"
      )
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id = "sidebar",
      # menuItem("AiRR Dashboard", tabName = "airr_dashboard", icon = icon("dashboard")),
      # menuItem("Prompt Performance", tabName = "prompt_performance", icon = icon("chart-line")),
      # menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar")),
      # menuItem("Profile", tabName = "profile", icon = icon("user")),
      menuItem("Comparisons", tabName = "comparisons", icon = icon("balance-scale"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico"),
      tags$title("AiRR"),
      tags$style(HTML(custom_css))
    ),
    
    useShinyjs(),
    
    # Login/Register UI
    conditionalPanel(
      condition = "output.logged_in == false",
      div(
        class = "login-container",
        div(
          class = "login-header",
          h2(textOutput("auth_title")),
          p(textOutput("auth_subtitle"))
        ),
        div(
          class = "login-body",
          uiOutput("auth_alert"),
          
          # Login Form
          conditionalPanel(
            condition = "input.show_register == false || input.show_register == null",
            textInput("customer_name", NULL, placeholder = "Username"),
            passwordInput("login_password", NULL, placeholder = "Password"),
            actionButton("login_btn", "Sign In", class = "btn-login"),
            div(
              class = "toggle-link",
              span("Don't have an account? "),
              a("Sign Up", onclick = "Shiny.setInputValue('show_register', true)")
            )
          ),
          
          # Register Form
          conditionalPanel(
            condition = "input.show_register == true",
            textInput("register_customer_name", NULL, placeholder = "Username"),
            textInput("register_email", NULL, placeholder = "Email"),
            passwordInput("register_password", NULL, placeholder = "Password"),
            passwordInput("register_password_confirm", NULL, placeholder = "Confirm Password"),
            actionButton("register_btn", "Create Account", class = "btn-register"),
            div(
              class = "toggle-link",
              span("Already have an account? "),
              a("Sign In", onclick = "Shiny.setInputValue('show_register', false)")
            )
          )
        )
      )
    ),
    
    # Dashboard UI (shown when logged in)
    conditionalPanel(
      condition = "output.logged_in == true",
      
      tabItems(
        # Dashboard Tab
        tabItem(
          tabName = "airr_dashboard",
          
          fluidRow(
            valueBoxOutput("metric_box_1", width = 4)
            # valueBoxOutput("metric_box_2", width = 3),
            # valueBoxOutput("metric_box_3", width = 3),
            # valueBoxOutput("metric_box_4", width = 3)
          ),
          
          fluidRow(
            box(
              title = "Change over time",
              status = "primary",
              solidHeader = TRUE,
              width = 8,
              withSpinner(
                plotlyOutput("timeseries_chart", height = "400px"),
                # h3('timeseries chart here'),
                type = 4,
                color = "#667eea"
              )
            ),
            
            box(
              title = "The 4 'Ps'",
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              withSpinner(
                plotlyOutput("spider_chart", height = "400px"),
                type = 4,
                color = "#667eea"
              )
            )
          )
          
          # fluidRow(
          #   box(
          #     title = "Recent Activity",
          #     status = "info",
          #     solidHeader = TRUE,
          #     width = 12,
          #     withSpinner(
          #       # DT::dataTableOutput("activity_table"),
          #       h3('output table here'),
          #       type = 4,
          #       color = "#667eea"
          #     )
          #   )
          # )
        ),
        
        # Performance Tab
        tabItem(
          tabName = "prompt_performance",
          
          fluidRow(
            box(
              title = "Submit prompt for tracking",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              textInput(
                "prompt_input",
                "Enter your prompt:",
                placeholder = "Type your prompt here...",
                width = "60%"
              ),
              selectInput(
                "competitor_select",
                "Select Competitor for comparison:",
                choices = NULL,  # Will be populated by server
                selected = NULL,
                multiple = TRUE,
                width = "40%"
              ),
              actionButton(
                "submit_prompt_btn",
                "Submit Prompt",
                icon = icon("paper-plane"),
                class = "btn-primary",
                style = "margin-top: 10px; padding: 10px 20px; font-weight: 600;"
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Your Tracked Queries",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              uiOutput("queries_summary")
            )
          ),
          
          # Dynamic query panels will be inserted here
          uiOutput("query_panels")
        ),
        
        # Analytics Tab
        tabItem(
          tabName = "analytics",
          
          fluidRow(
            box(
              title = "Multi-Metric Spider Chart",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              withSpinner(
                # plotlyOutput("detailed_spider", height = "500px"),
                h3('multi met spider chart'),
                type = 4,
                color = "#667eea"
              )
            ),
            
            box(
              title = "Trend Analysis",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              withSpinner(
                # plotlyOutput("trend_chart", height = "500px"),
                h3('trend line'),
                type = 4,
                color = "#667eea"
              )
            )
          )
        ),
        
        # Profile Tab
        tabItem(
          tabName = "profile",
          
          fluidRow(
            box(
              title = "User Profile",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              uiOutput("profile_info")
            ),
            
            box(
              title = "Account Statistics",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              h3('account stats area')
              # uiOutput("account_stats")
            )
          )
        ),
        tabItem(
          tabName = "comparisons",
          
          fluidRow(
            box(
              title = "Customer Rankings by AIRR Score",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              DTOutput("leaderboard_table")
            )
          ),
          # fluidRow(
          #   box(
          #     title = "AIRR Score Trends Over Time",
          #     status = "primary",
          #     solidHeader = TRUE,
          #     width = 12,
          #     collapsible = TRUE,
          #     plotlyOutput("leaderboard_chart", height = "500px")
          #   )
          # ),
          fluidRow(
            box(
              title = "Score Trends Over Time",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              tabsetPanel(
                id = "leaderboard_chart_tabs",
                type = "tabs",
                tabPanel(
                  "AIRR Score",
                  value = "airr",
                  plotlyOutput("leaderboard_chart_airr", height = "500px")
                ),
                tabPanel(
                  "Presence Score",
                  value = "presence",
                  plotlyOutput("leaderboard_chart_presence", height = "500px")
                ),
                tabPanel(
                  "Perception Score",
                  value = "perception",
                  plotlyOutput("leaderboard_chart_perception", height = "500px")
                ),
                tabPanel(
                  "Prestige Score",
                  value = "prestige",
                  plotlyOutput("leaderboard_chart_prestige", height = "500px")
                ),
                tabPanel(
                  "Persistence Score",
                  value = "persistence",
                  plotlyOutput("leaderboard_chart_persistence", height = "500px")
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Select Customers to Compare",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(4,
                       selectInput(
                         "compare_customer_1",
                         "Customer 1:",
                         choices = NULL,
                         selected = NULL
                       )
                ),
                column(4,
                       selectInput(
                         "compare_customer_2",
                         "Customer 2:",
                         choices = NULL,
                         selected = NULL
                       )
                ),
                column(4,
                       selectInput(
                         "compare_customer_3",
                         "Customer 3:",
                         choices = NULL,
                         selected = NULL
                       )
                )
              ),
              fluidRow(
                column(12,
                       plotlyOutput("spider_chart_compare", height = "600px"))
              )
            )
          ),
          fluidRow(
            box(
              title = "Add New Prompt for All Customers",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(
                  width = 9,
                  textInput(
                    "new_prompt_input",
                    NULL,  # No label since box has title
                    placeholder = "Enter your prompt here...",
                    width = "100%"
                  )
                ),
                column(
                  width = 3,
                  # Add margin to align button with input
                  tags$div(
                    style = "margin-top: 0px;",
                    actionButton(
                      "submit_prompt_to_all_btn",
                      "Add Prompt",
                      icon = icon("plus"),
                      class = "btn-primary",
                      width = "100%"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  selectInput(
                    "query_select",
                    "Choose a prompt to analyze:",
                    choices = NULL,
                    selected = NULL,
                    width = "100%"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  DTOutput("query_top10_table")
                )
              )
              # fluidRow(
              #   column(
              #     width = 12,
              #     tabsetPanel(
              #       id = "query_chart_tabs",
              #       type = "tabs",
              #       tabPanel(
              #         "AIRR Score",
              #         value = "airr",
              #         plotlyOutput("query_chart_airr", height = "500px")
              #       ),
              #       tabPanel(
              #         "Presence Score",
              #         value = "presence",
              #         plotlyOutput("query_chart_presence", height = "500px")
              #       ),
              #       tabPanel(
              #         "Perception Score",
              #         value = "perception",
              #         plotlyOutput("query_chart_perception", height = "500px")
              #       ),
              #       tabPanel(
              #         "Prestige Score",
              #         value = "prestige",
              #         plotlyOutput("query_chart_prestige", height = "500px")
              #       ),
              #       tabPanel(
              #         "Persistence Score",
              #         value = "persistence",
              #         plotlyOutput("query_chart_persistence", height = "500px")
              #       )
              #     )
              #   )
              # )
            )
          )
        )
      )
    )
  )
)




