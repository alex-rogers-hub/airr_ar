# ============================================
# Admin: Reset industry for one or more brands
# Run this manually as administrator
# ============================================

library(DBI)
library(RPostgres)
library(httr2)
library(dplyr)
library(tibble)

# ============================================
# Config — set these before running
# ============================================

# Set to a brand name to reset just one, or NULL to reset all brands
TARGET_BRAND <- "Nike"   # e.g. "Nike" or NULL for all

# Model to use for industry lookup
MODEL <- "gpt-4.1-mini"

# Number of API calls per brand to consensus the industry
REPS <- 10

# ============================================
# Connect
# ============================================

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = Sys.getenv("DB_PORT"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

on.exit(dbDisconnect(con), add = TRUE)

# ============================================
# Helper: look up industry for one brand
# ============================================

lookup_industry <- function(brand_name, reach_context = "", model = MODEL, reps = REPS) {
  
  reach_str <- if (nzchar(reach_context)) paste0(" (", reach_context, ")") else ""
  
  prompt_text <- paste0(
    'In what industry is the brand ', brand_name, reach_str,
    '? Give your answer in the form "', brand_name, ' is in the __ industry",',
    ' giving no other information'
  )
  
  cat(sprintf("  Sending %d API calls for: %s%s\n", reps, brand_name, reach_str))
  
  industry_raw <- lapply(seq_len(reps), function(i) {
    tryCatch({
      resp <- request("https://api.openai.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
          `Content-Type` = "application/json"
        ) |>
        req_body_json(list(
          model    = model,
          messages = list(list(role = "user", content = prompt_text)),
          temperature = 0.4,
          max_tokens  = 30
        )) |>
        req_perform()
      
      parsed <- resp_body_json(resp)
      trimws(parsed$choices[[1]]$message$content)
      
    }, error = function(e) {
      warning(paste("API call failed:", e$message))
      NA_character_
    })
  })
  
  # Parse — extract the industry portion from "[brand] is in the __ industry"
  prefix_len <- nchar(brand_name) + nchar(" is in the ")
  
  industry_summary <- industry_raw |>
    unlist() |>
    (\(x) x[!is.na(x)])() |>
    tibble(industry = x) |>
    mutate(
      industry_desc = substr(
        industry,
        prefix_len + 1,
        nchar(industry) - nchar(" industry")
      ),
      industry_desc = trimws(industry_desc)
    ) |>
    filter(nchar(industry_desc) > 0) |>
    count(industry_desc, sort = TRUE, name = "mentions") |>
    head(1)
  
  if (nrow(industry_summary) > 0) {
    return(industry_summary$industry_desc[1])
  } else {
    return(NA_character_)
  }
}

# ============================================
# Helper: build reach context string
# ============================================

build_reach_context <- function(reach, country = NULL, region = NULL, postcode = NULL) {
  if (is.null(reach) || is.na(reach)) return("")
  if (!is.null(country)  && is.na(country))  country  <- NULL
  if (!is.null(region)   && is.na(region))   region   <- NULL
  if (!is.null(postcode) && is.na(postcode)) postcode <- NULL
  
  switch(reach,
         "global"   = "",
         "national" = if (!is.null(country)  && nzchar(country))  country  else "",
         "regional" = if (!is.null(region)   && nzchar(region))   region   else "",
         "near_me"  = {
           parts <- c()
           if (!is.null(postcode) && nzchar(postcode)) parts <- c(parts, postcode)
           if (!is.null(country)  && nzchar(country))  parts <- c(parts, country)
           paste(parts, collapse = ", ")
         },
         ""
  )
}

# ============================================
# Get brands to update
# ============================================

if (!is.null(TARGET_BRAND)) {
  brands <- dbGetQuery(con,
                       "SELECT brand_id, brand_name, brand_reach, reach_country, reach_region, 
            reach_postcode, industry
     FROM dim_brand
     WHERE lower(brand_name) = lower($1)",
                       params = list(TARGET_BRAND))
} else {
  brands <- dbGetQuery(con,
                       "SELECT brand_id, brand_name, brand_reach, reach_country, reach_region,
            reach_postcode, industry
     FROM dim_brand
     ORDER BY brand_name")
}

if (nrow(brands) == 0) {
  cat("No brands found.\n")
  quit(save = "no")
}

cat(sprintf("\n=== Industry Reset: %d brand(s) to process ===\n\n", nrow(brands)))

# ============================================
# Process each brand
# ============================================

results <- data.frame(
  brand_name   = character(),
  old_industry = character(),
  new_industry = character(),
  updated      = logical(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(brands))) {
  brand_name   <- brands$brand_name[i]
  brand_id     <- brands$brand_id[i]
  old_industry <- brands$industry[i]
  reach        <- brands$brand_reach[i]  %||% "global"
  country      <- brands$reach_country[i]
  region       <- brands$reach_region[i]
  postcode     <- brands$reach_postcode[i]
  
  if (is.na(country))  country  <- NULL
  if (is.na(region))   region   <- NULL
  if (is.na(postcode)) postcode <- NULL
  
  cat(sprintf("[%d/%d] %s\n", i, nrow(brands), brand_name))
  cat(sprintf("  Current industry: %s\n", if (is.na(old_industry)) "(none)" else old_industry))
  
  reach_context <- build_reach_context(reach, country, region, postcode)
  
  new_industry <- tryCatch({
    lookup_industry(brand_name, reach_context)
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    NA_character_
  })
  
  if (is.na(new_industry) || !nzchar(new_industry)) {
    cat("  Could not determine industry — skipping\n\n")
    results <- rbind(results, data.frame(
      brand_name   = brand_name,
      old_industry = old_industry %||% NA_character_,
      new_industry = NA_character_,
      updated      = FALSE,
      stringsAsFactors = FALSE
    ))
    next
  }
  
  cat(sprintf("  New industry:     %s\n", new_industry))
  
  # Only update if changed
  if (!is.na(old_industry) && tolower(trimws(old_industry)) == tolower(trimws(new_industry))) {
    cat("  No change — skipping\n\n")
    results <- rbind(results, data.frame(
      brand_name   = brand_name,
      old_industry = old_industry,
      new_industry = new_industry,
      updated      = FALSE,
      stringsAsFactors = FALSE
    ))
    next
  }
  
  dbExecute(con,
            "UPDATE dim_brand SET industry = $1 WHERE brand_id = $2",
            params = list(new_industry, brand_id))
  
  cat("  \u2713 Updated\n\n")
  
  results <- rbind(results, data.frame(
    brand_name   = brand_name,
    old_industry = old_industry %||% NA_character_,
    new_industry = new_industry,
    updated      = TRUE,
    stringsAsFactors = FALSE
  ))
  
  # Brief pause between brands to avoid rate limiting
  if (i < nrow(brands)) Sys.sleep(1)
}

# ============================================
# Summary
# ============================================

cat("=== Summary ===\n")
cat(sprintf("Total brands processed: %d\n", nrow(results)))
cat(sprintf("Updated:                %d\n", sum(results$updated)))
cat(sprintf("Unchanged:              %d\n", sum(!results$updated & !is.na(results$new_industry))))
cat(sprintf("Failed:                 %d\n", sum(is.na(results$new_industry))))

cat("\nFull results:\n")
print(results)

