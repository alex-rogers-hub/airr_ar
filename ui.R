source("global.R")
source("custom_css_stuff.R")

# Source tab UI scripts
source("ui_scripts/ui_brand_overview.R")
source("ui_scripts/ui_prompt_overview.R")
source("ui_scripts/ui_account.R")
source("ui_scripts/ui_comparisons.R")
source("ui_scripts/ui_onboarding.R")
source("ui_scripts/ui_profiles.R")
source("ui_scripts/ui_pricing.R")

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
      menuItem("Customer Personas", tabName = "profiles", icon = icon("users")),
      menuItem("Account",           tabName = "account",         icon = icon("user-cog"))
    ),
    
    # Persistent brand card
    uiOutput("sidebar_brand_card"),
    
    uiOutput("linked_account_switcher_ui"),
    
    # Sticky scores — slides in when score cards scroll out of view
    div(
      id = "sidebar_sticky_scores",
      style = "overflow: hidden; max-height: 0; transition: max-height 0.3s ease;
             margin: 0 12px;",
      uiOutput("sticky_score_bar_ui")
    ),
    
    # Sticky leaderboard — slides in when leaderboard scrolls out of view
    div(
      id = "sidebar_sticky_leaderboard",
      style = "overflow: hidden; max-height: 0; transition: max-height 0.4s ease;
           margin: 0 12px;",
      uiOutput("sticky_leaderboard_ui")
    ),
    
    # Scroll watcher
    tags$script(HTML("
  (function() {
    var sidebar_scores      = document.getElementById('sidebar_sticky_scores');
    var sidebar_leaderboard = document.getElementById('sidebar_sticky_leaderboard');

    function attachObservers() {

      // ── Score cards observer ─────────────────────────────────────
      var score_sentinel = document.getElementById('score_cards_sentinel');
      if (score_sentinel && sidebar_scores) {
        var score_observer = new IntersectionObserver(function(entries) {
          entries.forEach(function(entry) {
            if (entry.isIntersecting) {
              sidebar_scores.style.maxHeight = '0';
            } else {
              sidebar_scores.style.maxHeight = '300px';
            }
          });
        }, { root: null, threshold: 0, rootMargin: '-60px 0px 0px 0px' });

        score_observer.observe(score_sentinel);
      } else {
        setTimeout(attachObservers, 500);
        return;
      }

      // ── Leaderboard observer ─────────────────────────────────────
      var ldb_sentinel = document.getElementById('leaderboard_sentinel');
      if (ldb_sentinel && sidebar_leaderboard) {
        var ldb_observer = new IntersectionObserver(function(entries) {
          entries.forEach(function(entry) {
            if (entry.isIntersecting) {
              sidebar_leaderboard.style.maxHeight = '0';
            } else {
              sidebar_leaderboard.style.maxHeight = '600px';
            }
          });
        }, { root: null, threshold: 0, rootMargin: '-60px 0px 0px 0px' });

        ldb_observer.observe(ldb_sentinel);
      }
    }

    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', attachObservers);
    } else {
      attachObservers();
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
      # tags$script(HTML("
      #   (function() {
      #     var params = new URLSearchParams(window.location.search);
      #     if (params.get('payment') === 'success') {
      #       Shiny.setInputValue('payment_status', 'success', {priority: 'event'});
      #       // Clean URL
      #       window.history.replaceState({}, '', window.location.pathname);
      #     }
      #   })();
      # ")),
      tags$script(HTML("
        (function() {
          var params = new URLSearchParams(window.location.search);
          var token  = params.get('reset_token');
          var payment = params.get('payment');
      
          function applyUrlParams() {
            if (typeof Shiny === 'undefined' || !Shiny.setInputValue) {
              setTimeout(applyUrlParams, 100);
              return;
            }
      
            if (token) {
              Shiny.setInputValue('url_reset_token',  token, {priority: 'event'});
              Shiny.setInputValue('show_reset_form',  true,  {priority: 'event'});
              Shiny.setInputValue('show_register',    false, {priority: 'event'});
              Shiny.setInputValue('show_forgot',      false, {priority: 'event'});
              window.history.replaceState({}, '', window.location.pathname);
            }
      
            if (payment === 'success') {
              Shiny.setInputValue('payment_status', 'success', {priority: 'event'});
              window.history.replaceState({}, '', window.location.pathname);
            }
          }
      
          // Start trying once DOM is ready
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', applyUrlParams);
          } else {
            applyUrlParams();
          }
        })();
      ")),
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
        $(document).on('keypress', '#forgot_email', function(e) {
          if (e.which == 13) {
            e.preventDefault();
            $('#forgot_btn').click();
          }
        });
      "))
    ),
    
    useShinyjs(),
    
    # Admin switcher bar — only renders for admin users
    uiOutput("admin_switcher_ui"),
    uiOutput("demo_switcher_ui"),
    
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
          
          # ── Sign in ──────────────────────────────────────────────
          conditionalPanel(
            condition = "input.show_register == false || input.show_register == null",
            conditionalPanel(
              condition = "input.show_forgot == false || input.show_forgot == null",
              textInput("login_email", NULL, placeholder = "Email"),
              passwordInput("login_password", NULL, placeholder = "Password"),
              actionButton("login_btn", "Sign In", class = "btn-login"),
              div(
                class = "toggle-link",
                span("Don't have an account? "),
                a("Sign Up",
                  onclick = "Shiny.setInputValue('show_register', true)")
              ),
              div(
                style = "text-align: center; margin-top: 10px;",
                a(
                  style = "font-size: 12px; color: #9E9E9E; cursor: pointer;",
                  onclick = "Shiny.setInputValue('show_forgot', true,
                             {priority:'event'})",
                  "Forgot your password?"
                )
              )
            ),
            
            # ── Forgot password ───────────────────────────────────
            conditionalPanel(
              condition = "input.show_forgot == true",
              div(
                style = "text-align: center; margin-bottom: 16px;",
                h4(style = "color: #2d3748; font-weight: 600;",
                   "Reset Password"),
                p(style = "color: #718096; font-size: 13px;",
                  "Enter your email and we'll send you a reset link.")
              ),
              uiOutput("forgot_alert"),
              textInput("forgot_email", NULL,
                        placeholder = "Email address", width = "100%"),
              actionButton("forgot_btn", "Send Reset Link",
                           class = "btn-login",
                           style = "width: 100%;"),
              div(
                class = "toggle-link",
                style = "margin-top: 12px;",
                a(
                  style = "color: #9E9E9E; cursor: pointer;",
                  onclick = "Shiny.setInputValue('show_forgot', false,
                             {priority:'event'})",
                  "\u2190 Back to login"
                )
              )
            )
          ),
          
          # ── Register ─────────────────────────────────────────────
          conditionalPanel(
            condition = "input.show_register == true",
            textInput("register_email", NULL, placeholder = "Email"),
            passwordInput("register_password", NULL, placeholder = "Password"),
            passwordInput("register_password_confirm", NULL,
                          placeholder = "Confirm Password"),
            actionButton("register_btn", "Create Account",
                         class = "btn-register"),
            div(
              class = "toggle-link",
              span("Already have an account? "),
              a("Sign In",
                onclick = "Shiny.setInputValue('show_register', false)")
            )
          ),
          
          # ── Reset password (from email link) ──────────────────────
          conditionalPanel(
            condition = "input.show_reset_form == true",
            div(
              style = "text-align: center; margin-bottom: 16px;",
              h4(style = "color: #2d3748; font-weight: 600;",
                 "Choose New Password"),
              p(style = "color: #718096; font-size: 13px;",
                "Must be 8+ characters with a capital letter, number and symbol.")
            ),
            uiOutput("reset_alert"),
            passwordInput("reset_password", NULL,
                          placeholder = "New password", width = "100%"),
            passwordInput("reset_password_confirm", NULL,
                          placeholder = "Confirm new password", width = "100%"),
            actionButton("reset_btn", "Set New Password",
                         class = "btn-login",
                         style = "width: 100%;")
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
    ),
    # ============================================
    # FOOTER
    # ============================================
    shinyjs::hidden(
      tags$footer(
        id = "app_footer",
        div(
          class = "footer-inner",
          div(
            class = "footer-logo-wrap",
            tags$img(
              src    = "AiRR_logo.jpg",
              height = "36px",
              alt    = "AiRR Logo",
              style  = "opacity: 0.9;"
            )
          ),
          div(
            class = "footer-text-wrap",
            p(class = "footer-tagline",
              "Quantifying how customers choose you"),
            p(class = "footer-copy",
              "\u00A9 2026 AI Reach & Rank LLC. All rights reserved.")
          )
        )
      )
    )
  )
)