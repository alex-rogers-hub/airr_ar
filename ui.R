source("global.R")
source("custom_css_stuff.R")

# Source tab UI scripts
source("ui_scripts/ui_brand_overview.R")
source("ui_scripts/ui_prompt_overview.R")
source("ui_scripts/ui_account.R")
source("ui_scripts/ui_comparisons.R")

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
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
      menuItem("Brand Overview", tabName = "brand_overview", icon = icon("building")),
      menuItem("Prompt Overview", tabName = "prompt_overview", icon = icon("comment-dots")),
      # menuItem("Comparisons", tabName = "comparisons", icon = icon("balance-scale")),
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
        tab_brand_overview,
        tab_prompt_overview,
        tab_account,
        tab_comparisons
      )
    )
  )
)