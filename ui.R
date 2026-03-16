source("global.R")
source("custom_css_stuff.R")

# Source tab UI scripts
source("ui_scripts/ui_brand_overview.R")
source("ui_scripts/ui_prompt_overview.R")
source("ui_scripts/ui_account.R")
source("ui_scripts/ui_comparisons.R")
source("ui_scripts/ui_onboarding.R")
source("ui_scripts/ui_profiles.R")

ui <- dashboardPage(
  skin = "blue",
  title = "AiRR",
  
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
      menuItem("Brand Overview",    tabName = "brand_overview",  icon = icon("building")),
      menuItem("Prompt Overview",   tabName = "prompt_overview", icon = icon("comment-dots")),
      menuItem("Customer Personas", tabName = "profiles",
               icon = icon("users"),
               badgeLabel = "Enterprise", badgeColor = "purple"),
      menuItem("Account",           tabName = "account",         icon = icon("user-cog"))
    ),
    
    # Persistent brand card
    uiOutput("sidebar_brand_card"),
    
    # Sticky scores — slides in when score cards scroll out of view
    div(
      id = "sidebar_sticky_scores",
      style = "overflow: hidden; max-height: 0; transition: max-height 0.3s ease;
             margin: 0 12px;",
      uiOutput("sticky_score_bar_ui")
    ),
    
    # Scroll watcher
    tags$script(HTML("
    (function() {
      var sidebar_scores = document.getElementById('sidebar_sticky_scores');
      if (!sidebar_scores) return;

      function attachObserver() {
        var sentinel = document.getElementById('score_cards_sentinel');
        if (!sentinel) {
          setTimeout(attachObserver, 500);
          return;
        }

        var observer = new IntersectionObserver(function(entries) {
          entries.forEach(function(entry) {
            if (entry.isIntersecting) {
              // Cards visible — collapse sidebar scores
              sidebar_scores.style.maxHeight = '0';
            } else {
              // Cards off screen — expand sidebar scores
              sidebar_scores.style.maxHeight = '300px';
            }
          });
        }, {
          root: null,
          threshold: 0,
          rootMargin: '-60px 0px 0px 0px'
        });

        observer.observe(sentinel);
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', attachObserver);
      } else {
        attachObserver();
      }
    })();
  "))
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico"),
      tags$title("AiRR"),
      tags$style(HTML(custom_css)),
      
      tags$script(HTML("
        $(document).ready(function() {
      
          // Create a single tooltip div appended to body
          var $tip = $('<div class=\"airr-tooltip\"></div>').appendTo('body');
      
          $(document).on('mouseenter', '[data-tooltip]', function(e) {
            var text = $(this).attr('data-tooltip');
            if (!text) return;
            $tip.text(text).show();
            positionTip(e);
          });
      
          $(document).on('mousemove', '[data-tooltip]', function(e) {
            positionTip(e);
          });
      
          $(document).on('mouseleave', '[data-tooltip]', function() {
            $tip.hide();
          });
      
          function positionTip(e) {
            var tipW = $tip.outerWidth();
            var tipH = $tip.outerHeight();
            var x    = e.clientX - tipW / 2;
            var y    = e.clientY + 18;
      
            // Keep within viewport horizontally
            var maxX = $(window).width() - tipW - 8;
            if (x < 8)    x = 8;
            if (x > maxX) x = maxX;
      
            // Flip above cursor if too close to bottom
            if (y + tipH > $(window).height() - 8) {
              y = e.clientY - tipH - 10;
            }
      
            $tip.css({ left: x, top: y });
          }
      
        });
      ")),
      
      # ── Enter key submits login / register forms ──────────────────────────
      tags$script(HTML("
        function submitLogin() {
          // Force Shiny to flush current input values before clicking
          var email = $('#login_email').val();
          var password = $('#login_password').val();
          Shiny.setInputValue('login_email', email, {priority: 'event'});
          Shiny.setInputValue('login_password_val', password, {priority: 'event'});
          setTimeout(function() {
            $('#login_btn').click();
          }, 50);
        }
        
        function submitRegister() {
          setTimeout(function() {
            $('#register_btn').click();
          }, 50);
        }
      
        $(document).on('keypress', '#login_email', function(e) {
          if (e.which == 13) {
            e.preventDefault();
            submitLogin();
          }
        });
        $(document).on('keypress', '#login_password', function(e) {
          if (e.which == 13) {
            e.preventDefault();
            submitLogin();
          }
        });
        $(document).on('keypress', '#register_password_confirm', function(e) {
          if (e.which == 13) {
            e.preventDefault();
            submitRegister();
          }
        });
      "))
    ),
    
    useShinyjs(),
    
    # Admin switcher bar — only renders for admin users
    uiOutput("admin_switcher_ui"),
    
    # ============================================
    # LOGIN / REGISTER
    # ============================================
    div(
      id = "login_panel",
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
    # ONBOARDING
    # ============================================
    div(
      id = "onboarding_panel",
      style = "display: none;",   # hidden by default, shinyjs controls it
      div(
        style = "padding: 20px;",
        ui_onboarding()
      )
    ),
    
    # ============================================
    # MAIN APP
    # ============================================
    div(
      id = "main_app_panel",
      style = "display: none;",   # hidden by default, shinyjs controls it
      tabItems(
        tab_brand_overview,
        tab_prompt_overview,
        tab_profiles,
        tab_account,
        tab_comparisons
      )
    )
  )
)