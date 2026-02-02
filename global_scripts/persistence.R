# persistence.R
# AiRR POV - Persistence Measurement
# Measures temporal stability of visibility
#
# Formula: Persistence = 100 × (1 - CoeffVariation)
# Where: CoeffVariation = standard_deviation(scores) / mean(scores)

library(dplyr)
library(purrr)
library(tibble)

# Constants ---------------------------------------------------------------

# PERSISTENCE_WEIGHTS <- list(
#   time_stability = 0.7,
#   topic_consistency = 0.3
# )


# Helper Functions --------------------------------------------------------

#' Calculate coefficient of variation
#' @param values Numeric vector of scores
#' @return Numeric CV or NULL if insufficient data
calculate_coefficient_of_variation <- function(values) {
  
  # Remove NA values
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
#' @param historical_presence Numeric vector of historical presence scores
#' @return List with persistence score and details
calculate_persistence_score <- function(brand_name,
                                        historical_presence) {
  
  presence_values <- (historical_presence %>%
    arrange(date))$overall_score
  
  daily_delta <- 100 * ((presence_values %>%
    tail(1)) -
    (presence_values %>%
    tail(2) %>%
    head(1))) /
    (presence_values %>%
       tail(2) %>%
       head(1))
  
  # Calculate time stability (coefficient of variation)
  cv <- calculate_coefficient_of_variation(presence_values)
  
  time_stability <- if (!is.null(cv)) {
    max(0, min(100, 100 * (1 - cv)))
  } else {
    100
  }
  
  # Calculate overall persistence
  # persistence <- (
  #   PERSISTENCE_WEIGHTS$time_stability * time_stability +
  #     PERSISTENCE_WEIGHTS$topic_consistency * topic_consistency
  # )
  
  persistence <- time_stability
  
  # Interpretation
  interpretation <- case_when(
    persistence >= 85 ~ "Excellent - Highly stable and consistent presence",
    persistence >= 70 ~ "Good - Stable presence with minor variations",
    persistence >= 55 ~ "Fair - Moderate stability",
    persistence >= 40 ~ "Poor - Significant fluctuations",
    TRUE ~ "Very Poor - Highly unstable presence"
  )
  
  # Return persistence score
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
#' Calculate prestige score directly from prompt texts
#' @param brand_name Character, name of brand to score
#' @return List with score, prompts, responses, and metadata
calculate_daily_persistence <- function(brand_name, run_date) {
  
  cust_id = dbGetQuery(con, paste0("select customer_id from dim_customer where lower(customer_name) = '",tolower(brand_name),"';"))$customer_id
  # prestige_scores <- dbGetQuery(con, paste0('select * from fact_prestige_history
  #                                            where customer_id = ',cust_id,';'))
  # perception_scores <- dbGetQuery(con, paste0('select * from fact_perception_history
  #                                            where customer_id = ',cust_id,';'))
  presence_scores <- dbGetQuery(con, paste0("select * from fact_presence_history
                                             where customer_id = ", cust_id, "
                                             and date <= '", run_date, "';"))
  
  if(nrow(presence_scores) < 5){
    persistence_out <- list(
      brand_name = brand_name,
      score = 100,
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
  
  presence_scores <- presence_history
  
  persistence_out <- calculate_persistence_score('placeholder',
                                                 presence_scores)
  
  return(persistence_out$score)
}

