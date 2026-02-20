# persistence.R
# AiRR POV - Persistence Measurement
# Measures temporal stability of visibility
#
# Formula: Persistence = (1 - CoeffVariation) × latest_presence_score
# Where: CoeffVariation = standard_deviation(scores) / mean(scores)

library(dplyr)
library(purrr)
library(tibble)

# Helper Functions --------------------------------------------------------

#' Calculate coefficient of variation
#' @param values Numeric vector of scores
#' @return Numeric CV or NULL if insufficient data
calculate_coefficient_of_variation <- function(values) {
  
  values <- values[!is.na(values)]
  
  if (length(values) < 2) {
    return(NULL)
  }
  
  avg <- mean(values)
  
  if (avg == 0) {
    return(NULL)
  }
  
  std <- sd(values)
  cv <- std / avg
  
  return(cv)
}


#' Calculate persistence score from historical presence scores
#' @param brand_name Character, name of brand
#' @param historical_presence Data frame with date and overall_score columns
#' @return List with persistence score and details
calculate_persistence_score <- function(brand_name,
                                        historical_presence) {
  
  presence_values <- (historical_presence %>%
                        arrange(date))$overall_score
  
  # Most recent presence score
  latest_presence <- tail(presence_values, 1)
  
  daily_delta <- 100 * ((presence_values %>%
                           tail(1)) -
                          (presence_values %>%
                             tail(2) %>%
                             head(1))) /
    (presence_values %>%
       tail(2) %>%
       head(1))
  
  # Calculate coefficient of variation
  cv <- calculate_coefficient_of_variation(presence_values)
  
  # Persistence = (1 - CV) × latest presence score
  persistence <- if (!is.null(cv)) {
    max(0, (1 - cv) * latest_presence)
  } else {
    latest_presence
  }
  
  # Interpretation (thresholds relative to latest presence)
  stability_ratio <- if (latest_presence > 0) persistence / latest_presence else 0
  
  interpretation <- case_when(
    stability_ratio >= 0.85 ~ "Excellent - Highly stable and consistent presence",
    stability_ratio >= 0.70 ~ "Good - Stable presence with minor variations",
    stability_ratio >= 0.55 ~ "Fair - Moderate stability",
    stability_ratio >= 0.40 ~ "Poor - Significant fluctuations",
    TRUE ~ "Very Poor - Highly unstable presence"
  )
  
  persistence_out <- list(
    brand_name = brand_name,
    score = round(persistence, 2),
    coefficient_of_variation = if (!is.null(cv)) round(cv, 4) else 0.0,
    interpretation = interpretation,
    daily_perc_change = daily_delta,
    timestamp = Sys.time()
  )
  
  return(persistence_out)
}


# Main Interface Function -------------------------------------------------

calculate_daily_persistence <- function(brand_name, run_date) {
  
  brand_id = dbGetQuery(con, 
                        "SELECT brand_id FROM dim_brand WHERE lower(brand_name) = lower($1);",
                        params = list(brand_name))$brand_id
  
  presence_scores <- dbGetQuery(con, 
                                "SELECT * FROM fact_presence_history
   WHERE brand_id = $1 AND date <= $2;",
                                params = list(brand_id, run_date))
  
  if(nrow(presence_scores) < 5){
    # Not enough history — persistence = latest presence score (no penalty)
    latest <- if (nrow(presence_scores) > 0) {
      tail((presence_scores %>% arrange(date))$overall_score, 1)
    } else {
      0
    }
    
    persistence_out <- list(
      brand_name = brand_name,
      score = latest,
      coefficient_of_variation = 0,
      interpretation = "Not enough data",
      daily_perc_change = 0,
      timestamp = Sys.time()
    )
  } else {
    persistence_out <- calculate_persistence_score(brand_name,
                                                   presence_scores)
  }
  
  return(persistence_out)
}

calculate_daily_persistence_sep <- function(presence_history) {
  
  presence_scores <- presence_history %>%
    rename(overall_score = presence_score)
  
  persistence_out <- calculate_persistence_score('placeholder',
                                                 presence_scores)
  
  return(persistence_out$score)
}