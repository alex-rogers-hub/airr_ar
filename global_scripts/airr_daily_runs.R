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
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

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
verify_user <- function(customer_name, password) {
  query <- "SELECT customer_id, customer_name, password_hash, email 
            FROM dim_customer 
            WHERE customer_name = $1"
  
  result <- dbGetQuery(pool, query, params = list(customer_name))
  
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  if (result$password_hash == hash_password(password)) {
    return(result)
  }
  
  return(NULL)
}

# Helper function to create new user
create_user <- function(customer_name, email, password) {
  tryCatch({
    query <- "INSERT INTO dim_customer (customer_name, date_added, email, password_hash) 
              VALUES ($1, $2, $3, $4) 
              RETURNING customer_id"
    
    result <- dbGetQuery(
      pool, 
      query, 
      params = list(customer_name, Sys.Date(), email, hash_password(password))
    )
    
    return(result$customer_id)
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
daily_refresh_loop()
daily_prompt_loop()

