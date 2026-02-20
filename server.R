source("global.R")
source("custom_css_stuff.R")

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    logged_in = FALSE,
    login_id = NULL,
    email = NULL,
    brand_id = NULL,
    brand_name = NULL,
    auth_message = NULL,
    auth_type = NULL,
    queries_refresh = 0,
    brands_refresh = 0 
  )
  
  # Source server sub-scripts
  source("server_scripts/server_auth.R", local = TRUE)
  source("server_scripts/server_dashboard.R", local = TRUE)
  source("server_scripts/server_queries.R", local = TRUE)
  source("server_scripts/server_leaderboard.R", local = TRUE)
  source("server_scripts/server_compare.R", local = TRUE)
  source("server_scripts/server_profile.R", local = TRUE)
  source("server_scripts/server_account.R", local = TRUE)
  
}