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
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar")),
      menuItem("Profile", tabName = "profile", icon = icon("user"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
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
            textInput("customer_name", NULL, placeholder = "Username"),
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
          tabName = "dashboard",
          
          fluidRow(
            valueBoxOutput("metric_box_1", width = 3),
            valueBoxOutput("metric_box_2", width = 3),
            valueBoxOutput("metric_box_3", width = 3),
            valueBoxOutput("metric_box_4", width = 3)
          ),
          
          fluidRow(
            box(
              title = "Performance Overview",
              status = "primary",
              solidHeader = TRUE,
              width = 8,
              withSpinner(
                # plotlyOutput("timeseries_chart", height = "400px"),
                h3('timeseries chart here'),
                type = 4,
                color = "#667eea"
              )
            ),
            
            box(
              title = "Skills Radar",
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              withSpinner(
                # plotlyOutput("spider_chart", height = "400px"),
                h3('spider chart here'),
                type = 4,
                color = "#667eea"
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Recent Activity",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              withSpinner(
                # DT::dataTableOutput("activity_table"),
                h3('output table here'),
                type = 4,
                color = "#667eea"
              )
            )
          )
        ),
        
        # Performance Tab
        tabItem(
          tabName = "performance",
          
          fluidRow(
            box(
              title = "Metric Selection",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              selectInput(
                "metric_selector",
                "Select Metrics to Display:",
                choices = c('choice1','choice2'),
                multiple = TRUE,
                width = "100%"
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Time Series Analysis",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              withSpinner(
                # plotlyOutput("detailed_timeseries", height = "500px"),
                h3('timeseries plot'),
                type = 4,
                color = "#667eea"
              )
            )
          )
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
        )
      )
    )
  )
)




