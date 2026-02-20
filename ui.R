source("global.R")
source("custom_css_stuff.R")

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    # title = "AiRR",
    title = tags$a(
      href = "#",
      tags$img(src = "AiRR_logo.jpg", height = "40px", style = "margin-top: -5px;"),
      style = "text-decoration: none;"
    ),
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
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Comparisons", tabName = "comparisons", icon = icon("balance-scale")),
      menuItem("Account", tabName = "account", icon = icon("user-cog"))
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
    
    # ============================================
    # LOGIN / REGISTER
    # ============================================
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
          
          conditionalPanel(
            condition = "input.show_register == false || input.show_register == null",
            textInput("login_email", NULL, placeholder = "Email"),
            passwordInput("login_password", NULL, placeholder = "Password"),
            actionButton("login_btn", "Sign In", class = "btn-login"),
            div(
              class = "toggle-link",
              span("Don't have an account? "),
              a("Sign Up", onclick = "Shiny.setInputValue('show_register', true)")
            )
          ),
          
          conditionalPanel(
            condition = "input.show_register == true",
            textInput("register_brand_name", NULL, placeholder = "Brand Name"),
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
    
    # ============================================
    # MAIN APP (logged in)
    # ============================================
    conditionalPanel(
      condition = "output.logged_in == true",
      
      tabItems(
        
        # ============================================
        # DASHBOARD TAB
        # ============================================
        tabItem(
          tabName = "dashboard",
          # --- Score Cards Row ---
          uiOutput("dash_score_cards_row"),
          br(),
          
          # --- Performance Overview: Trend + Spider ---
          fluidRow(
            column(
              width = 12,
              div(
                class = "box",
                style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); padding: 20px;",
                
                # Header with download
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; 
                           margin-bottom: 15px;",
                  h4(style = "margin: 0; font-weight: 600; color: #2d3748;", 
                     "Performance Overview"),
                  downloadButton("download_brand_data", "Export Data",
                                 class = "btn-download")
                ),
                # Charts side by side
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
          
          # --- Rankings Table ---
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
                  DTOutput("dash_rankings_table"),
                  type = 4, color = "#667eea"
                )
              )
            )
          ),
          
          br(),
          
          # --- Prompt Performance ---
          fluidRow(
            column(
              width = 12,
              div(
                class = "box",
                style = "border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.06); padding: 20px;",
                
                # Header with query selector and download
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; 
                           margin-bottom: 15px; flex-wrap: wrap; gap: 10px;",
                  h4(style = "margin: 0; font-weight: 600; color: #2d3748;", 
                     "Prompt Performance"),
                  div(
                    style = "display: flex; gap: 10px; align-items: center;",
                    div(
                      style = "width: 350px;",
                      selectInput("dash_query_select", NULL,
                                  choices = NULL, selected = NULL, width = "100%")
                    ),
                    downloadButton("download_prompt_data", "Export",
                                   class = "btn-download")
                  )
                ),
                
                # Query table
                withSpinner(
                  DTOutput("dash_query_table"),
                  type = 4, color = "#667eea"
                ),
                
                br(),
                
                # Query charts
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
              )
            )
          )
        ),
        
        # ============================================
        # ACCOUNT TAB
        # ============================================
        tabItem(
          tabName = "account",
          
          # Top row: Profile card + Usage gauges + Upgrade
          fluidRow(
            column(
              width = 3,
              uiOutput("account_profile_card")
            ),
            column(
              width = 3,
              uiOutput("account_brand_gauge")
            ),
            column(
              width = 3,
              uiOutput("account_query_gauge")
            ),
            column(
              width = 3,
              uiOutput("account_upgrade_card")
            )
          ),
          
          br(),
          
          # Competitor brands + Tracked prompts
          fluidRow(
            box(
              title = NULL, width = 6,
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                h4(style = "margin: 0; font-weight: 600; color: #2d3748;", "Competitor Brands"),
                uiOutput("account_brand_slot_badge")
              ),
              fluidRow(
                column(8,
                       textInput("add_competitor_brand_input", NULL,
                                 placeholder = "Enter a competitor brand name...", width = "100%")),
                column(4,
                       tags$div(style = "margin-top: 0px;",
                                actionButton("add_competitor_btn", "Add", icon = icon("plus"),
                                             class = "btn-primary", width = "100%")))
              ),
              hr(style = "margin: 12px 0; border-color: #f0f0f0;"),
              uiOutput("account_competitor_list")
            ),
            
            box(
              title = NULL, width = 6,
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                h4(style = "margin: 0; font-weight: 600; color: #2d3748;", "Tracked Prompts"),
                uiOutput("account_query_slot_badge")
              ),
              fluidRow(
                column(8,
                       textInput("add_query_input", NULL,
                                 placeholder = "Enter a prompt to track...", width = "100%")),
                column(4,
                       tags$div(style = "margin-top: 0px;",
                                actionButton("add_query_btn", "Add", icon = icon("plus"),
                                             class = "btn-primary", width = "100%")))
              ),
              hr(style = "margin: 12px 0; border-color: #f0f0f0;"),
              uiOutput("account_query_list")
            )
          )
        ),
        
        # ============================================
        # COMPARISONS TAB
        # ============================================
        tabItem(
          tabName = "comparisons",
          
          fluidRow(
            box(
              title = "Brand Rankings by AIRR Score",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              DTOutput("leaderboard_table")
            )
          ),
          
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
                tabPanel("AIRR Score",
                         plotlyOutput("leaderboard_chart_airr", height = "500px")),
                tabPanel("Presence Score",
                         plotlyOutput("leaderboard_chart_presence", height = "500px")),
                tabPanel("Perception Score",
                         plotlyOutput("leaderboard_chart_perception", height = "500px")),
                tabPanel("Prestige Score",
                         plotlyOutput("leaderboard_chart_prestige", height = "500px")),
                tabPanel("Persistence Score",
                         plotlyOutput("leaderboard_chart_persistence", height = "500px"))
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Select Brands to Compare",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(4, selectInput("compare_customer_1", "Brand 1:",
                                      choices = NULL, selected = NULL)),
                column(4, selectInput("compare_customer_2", "Brand 2:",
                                      choices = NULL, selected = NULL)),
                column(4, selectInput("compare_customer_3", "Brand 3:",
                                      choices = NULL, selected = NULL))
              ),
              fluidRow(
                column(12, plotlyOutput("spider_chart_compare", height = "600px"))
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Add New Prompt for All Brands",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(
                  width = 9,
                  textInput("new_prompt_input", NULL,
                            placeholder = "Enter your prompt here...",
                            width = "100%")
                ),
                column(
                  width = 3,
                  tags$div(
                    style = "margin-top: 0px;",
                    actionButton("submit_prompt_to_all_btn", "Add Prompt",
                                 icon = icon("plus"), class = "btn-primary",
                                 width = "100%")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  selectInput("query_select", "Choose a prompt to analyze:",
                              choices = NULL, selected = NULL, width = "100%")
                )
              ),
              fluidRow(
                column(width = 12, DTOutput("query_top10_table"))
              ),
              fluidRow(
                column(
                  width = 12,
                  tabsetPanel(
                    id = "query_chart_tabs",
                    type = "tabs",
                    tabPanel("AIRR Score",
                             plotlyOutput("query_chart_airr", height = "500px")),
                    tabPanel("Presence Score",
                             plotlyOutput("query_chart_presence", height = "500px")),
                    tabPanel("Perception Score",
                             plotlyOutput("query_chart_perception", height = "500px")),
                    tabPanel("Prestige Score",
                             plotlyOutput("query_chart_prestige", height = "500px")),
                    tabPanel("Persistence Score",
                             plotlyOutput("query_chart_persistence", height = "500px"))
                  )
                )
              )
            )
          )
        )
        
      ) # end tabItems
    ) # end conditionalPanel logged_in
  ) # end dashboardBody
) # end dashboardPage