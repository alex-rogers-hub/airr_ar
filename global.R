library(reticulate)
library(DBI)
library(RPostgres)
library(httr)
library(httr2)
library(jsonlite)
library(glue)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(fmsb) # for radar charts
library(digest) # for password hashing
library(DT)
library(zoo)
library(pool)
library(shinycssloaders)
library(future)
library(promises)

# ============================================
# Brand Theme (change these to update all colours)
# ============================================
BRAND_THEME <- list(
  # Primary colours from logo
  gold = "#D4A843",
  gold_dark = "#B8922E",
  gold_light = "#E8C96A",
  
  # Dark palette
  bg_dark = "#1A1A1A",
  surface_dark = "#2D2D2D",
  surface_mid = "#3A3A3A",
  
  # Accent
  red = "#C0392B",
  red_light = "#E74C3C",
  
  # Text
  text_light = "#F5F5F0",
  text_muted = "#9E9E9E",
  text_dark = "#1A1A1A",
  
  # Functional colours
  success = "#27AE60",
  warning = "#F39C12",
  info = "#3498DB",
  danger = "#C0392B",
  
  # Score card colours (4 P's)
  presence = "#27AE60",
  perception = "#D4A843",
  prestige = "#8E44AD",
  persistence = "#2980B9",
  
  # Competitor chart colours (distinct, professional)
  comp_palette = c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6", 
                   "#E67E22", "#1ABC9C", "#34495E", "#F39C12",
                   "#16A085", "#C0392B"),
  
  # Chart
  main_line = "#D4A843",
  grid = "rgba(255, 255, 255, 0.08)",
  
  # Gradients
  gradient_primary = "linear-gradient(135deg, #D4A843 0%, #B8922E 100%)",
  gradient_dark = "linear-gradient(135deg, #2D2D2D 0%, #1A1A1A 100%)",
  gradient_header = "linear-gradient(135deg, #1A1A1A 0%, #2D2D2D 100%)"
)

setwd('/home/aarogers/AiRR')
# setwd('/srv/shiny-server/AiRR')
print(getwd())
# Color palette
app_colors <- list(
  primary = "#2C3E50",
  secondary = "#3498DB",
  success = "#27AE60",
  warning = "#F39C12",
  danger = "#E74C3C",
  light = "#ECF0F1",
  dark = "#0b1117"
)

app_col2 <- list(
  dark_a = "#0b1117",
  turq = "#00ffd9",
  blue_mid = "#5348f2",
  pink_r = "#ff0080"
)


# Custom theme for plotly
plotly_theme <- list(
  font = list(family = "Segoe UI, Arial, sans-serif"),
  plot_bgcolor = "#FFFFFF",
  paper_bgcolor = "#FFFFFF",
  colorway = unlist(app_colors[2:6])
)

# Create connection
# con <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = Sys.getenv("DB_NAME"),
#   host = Sys.getenv("DB_HOST"),
#   port = Sys.getenv("DB_PORT"),
#   user = Sys.getenv("DB_USER"),
#   password = Sys.getenv("DB_PASSWORD")
# )

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

onStop(function() {
  poolClose(pool)
})

# Helper function to hash passwords
hash_password <- function(password) {
  digest(password, algo = "sha256")
}

# Helper function to verify user credentials
verify_user <- function(email, password) {
  query <- "SELECT login_id, email, password_hash 
            FROM dim_user 
            WHERE email = $1"
  
  result <- dbGetQuery(pool, query, params = list(email))
  
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  if (result$password_hash == hash_password(password)) {
    return(result)
  }
  
  return(NULL)
}

# Helper function to create new user
create_user <- function(email, password) {
  tryCatch({
    query <- "INSERT INTO dim_user (date_added, email, password_hash) 
              VALUES ($1, $2, $3) 
              RETURNING login_id"
    
    result <- dbGetQuery(
      pool, 
      query, 
      params = list(Sys.Date(), email, hash_password(password))
    )
    
    return(result$login_id)
  }, error = function(e) {
    return(NULL)
  })
}




OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")

# workflow_example.R
source("global_scripts/chatgpt_functions.R")
source("global_scripts/queries_send_return.R")
source("global_scripts/presence.R")
source("global_scripts/perception.R")
source("global_scripts/prestige.R")
source("global_scripts/persistence.R")
source("global_scripts/full_airr_score.R")
source("global_scripts/upload_functions.R")
source("global_scripts/app_helper_functions.R")

# running the below runs by default in "gpt-4o-mini". Add "gpt-4o" or other model name to change this
# daily_refresh_loop()
# daily_prompt_loop()

