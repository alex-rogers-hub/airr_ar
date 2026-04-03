# presence.R
# AiRR POV - Presence Measurement
# Measures brand visibility frequency in AI responses
#
# Formula: Presence = 100 × (Σ ωᵢ × Iᵢ) / (Σ ωᵢ)

library(stringr)
library(dplyr)
library(purrr)
library(tibble)

# Constants ---------------------------------------------------------------

#' Alternative: Linear position weighting
#' @param first_position Integer, character position
#' @param text_length Integer, total text length
#' @param mention_count Integer, number of mentions
#' @return Numeric score 0-1
calculate_position_weight_linear <- function(first_position,
                                             text_length,
                                             mention_count) {
  
  if (is.na(first_position) || text_length == 0) {
    return(0)
  }
  
  normalized_position <- first_position / text_length
  
  # Linear decay: 
  # Position 0% → 1.0
  # Position 50% → 0.5
  # Position 100% → 0.0
  position_score <- 1.0 - normalized_position
  
  # Frequency bonus
  frequency_bonus <- min(0.3, (mention_count - 1) * 0.1)
  
  total_score <- min(1.0, position_score + frequency_bonus)
  
  return(total_score)
}

#' Calculate position-based weight (earlier = higher score)
#' @param first_position Integer, character position of first mention
#' @param text_length Integer, total length of text
#' @param mention_count Integer, number of mentions
#' @return Numeric score 0-1
calculate_position_weight <- function(first_position, 
                                      text_length, 
                                      mention_count) {
  
  if (is.na(first_position) || text_length == 0) {
    return(0)
  }
  
  # Normalize position (0 = start, 1 = end)
  normalized_position <- first_position / text_length
  
  # Position score (exponential decay - earlier is much better)
  # First 10% → score ~1.0
  # First 25% → score ~0.8
  # First 50% → score ~0.5
  # Last 50% → score ~0.2
  position_score <- exp(-2 * normalized_position)
  
  # Frequency bonus (diminishing returns)
  # 1 mention → 0
  # 2 mentions → +0.15
  # 3 mentions → +0.25
  # 5+ mentions → +0.35
  frequency_bonus <- min(0.35, log10(mention_count + 1) * 0.4)
  
  # Combined score (capped at 1.0)
  total_score <- min(1.0, position_score + frequency_bonus)
  
  return(total_score)
}

#' Detect brand mentions and find positions
#' @param text Character string to search
#' @param brand_name List of regex patterns
#' @return List with mention details
detect_brand_with_position <- function(text, brand_name) {
  
  if (is.na(text) || nchar(text) == 0) {
    return(list(
      brand_mentioned = FALSE,
      first_position  = NA_integer_,
      mention_count   = 0,
      all_positions   = integer(0)
    ))
  }
  
  text_lower    <- tolower(sanitise_text(text))
  all_positions <- integer(0)
  
  for (pattern in brand_name) {
    matches <- str_locate_all(
      text_lower,
      regex(tolower(sanitise_text(pattern)), ignore_case = TRUE)
    )[[1]]
    
    if (nrow(matches) > 0) {
      all_positions <- c(all_positions, matches[, "start"])
    }
  }
  
  all_positions <- unique(all_positions)
  all_positions <- sort(all_positions)
  
  list(
    brand_mentioned = length(all_positions) > 0,
    first_position  = if (length(all_positions) > 0) all_positions[1] else NA_integer_,
    mention_count   = length(all_positions),
    all_positions   = all_positions
  )
}

#' Calculate presence score from response groups with position weighting
#' @param brand_name Character, name of brand to search for
#' @param brand_aliases Character vector, alternative names for brand
#' @param presence_responses List of response groups from all_queries
#' @return List with presence score and details
calculate_presence_from_responses <- function(brand_names,  # now a vector
                                              presence_responses) {
  
  presence_results <- tibble(
    group_name        = character(),
    response_num      = integer(),
    response_text     = character(),
    brand_mentioned   = logical(),
    first_position    = integer(),
    mention_count     = integer(),
    position_score    = numeric(),
    weighted_presence = numeric()
  )
  
  for (group_name in names(presence_responses)) {
    group_data <- presence_responses[[group_name]]$data
    
    for (i in seq_along(group_data)) {
      response <- group_data[i]
      
      if (is.na(response) || nchar(response) == 0) {
        presence_results <- presence_results %>%
          add_row(
            group_name = group_name, response_num = i,
            response_text = response, brand_mentioned = FALSE,
            first_position = NA_integer_, mention_count = 0,
            position_score = 0, weighted_presence = 0
          )
        next
      }
      
      # Check all brand names / aliases — take best result
      best <- list(brand_mentioned = FALSE,
                   first_position  = NA_integer_,
                   mention_count   = 0)
      
      for (search_name in brand_names) {
        result <- detect_brand_with_position(response, search_name)
        if (result$brand_mentioned) {
          if (!best$brand_mentioned ||
              (!is.na(result$first_position) &&
               (is.na(best$first_position) ||
                result$first_position < best$first_position))) {
            best$brand_mentioned <- TRUE
            best$first_position  <- result$first_position
            best$mention_count   <- best$mention_count + result$mention_count
          }
        }
      }
      
      position_score <- if (best$brand_mentioned) {
        calculate_position_weight(
          first_position = best$first_position,
          text_length    = nchar(response),
          mention_count  = best$mention_count
        )
      } else 0
      
      presence_results <- presence_results %>%
        add_row(
          group_name        = group_name,
          response_num      = i,
          response_text     = unlist(response),
          brand_mentioned   = best$brand_mentioned,
          first_position    = best$first_position,
          mention_count     = best$mention_count,
          position_score    = position_score,
          weighted_presence = position_score
        )
    }
  }
  
  total_responses      <- nrow(presence_results)
  simple_mention_rate  <- mean(presence_results$brand_mentioned) * 100
  weighted_presence_score <- mean(presence_results$weighted_presence) * 100
  
  by_question <- presence_results %>%
    group_by(group_name) %>%
    summarise(
      mention_rate       = mean(brand_mentioned),
      avg_position_score = mean(position_score),
      avg_first_position = mean(first_position, na.rm = TRUE),
      total_mentions     = sum(mention_count),
      .groups = "drop"
    ) %>%
    arrange(desc(mention_rate))
  
  mention_variance <- sd(presence_results$weighted_presence)
  
  interpretation <- case_when(
    weighted_presence_score >= 80 ~ "Excellent - Strong, prominent presence",
    weighted_presence_score >= 65 ~ "Very Good - Consistent presence with good positioning",
    weighted_presence_score >= 50 ~ "Good - Regular presence",
    weighted_presence_score >= 35 ~ "Fair - Moderate presence",
    weighted_presence_score >= 20 ~ "Weak - Infrequent presence",
    TRUE                          ~ "Very Weak - Minimal presence"
  )
  
  list(
    brand_name              = brand_names[1],
    overall_score           = round(weighted_presence_score, 2),
    simple_mention_rate     = round(simple_mention_rate, 2),
    total_responses         = total_responses,
    responses_with_mentions = sum(presence_results$brand_mentioned),
    by_question             = by_question,
    detailed_results        = presence_results,
    interpretation          = interpretation,
    stability               = round(100 - min(100, mention_variance * 100), 2),
    timestamp               = Sys.time()
  )
}

#' Calculate presence score from response groups with position weighting
#' @param brand_name Character, name of brand to search for
#' @param brand_aliases Character vector, alternative names for brand
#' @param presence_responses List of response groups from all_queries
#' @return List with presence score and details
presence_prompt_calc <- function(brand_names,   # now accepts vector
                                 presence_responses) {
  
  responses <- presence_responses
  responses$responses <- sanitise_text(responses$responses)
  
  presence_results <- tibble(
    response_num    = integer(),
    response_text   = character(),
    brand_mentioned = logical(),
    first_position  = integer(),
    mention_count   = integer(),
    position_score  = numeric(),
    weighted_presence = numeric()
  )
  
  for (i in 1:nrow(responses)) {
    
    # Check for any of the brand names / aliases
    best_result <- list(
      brand_mentioned = FALSE,
      first_position  = NA_integer_,
      mention_count   = 0
    )
    
    for (search_name in brand_names) {
      result <- detect_brand_with_position(responses$responses[i], search_name)
      if (result$brand_mentioned) {
        if (!best_result$brand_mentioned ||
            (!is.na(result$first_position) &&
             (is.na(best_result$first_position) ||
              result$first_position < best_result$first_position))) {
          best_result$brand_mentioned <- TRUE
          best_result$first_position  <- result$first_position
          best_result$mention_count   <- best_result$mention_count +
            result$mention_count
        }
      }
    }
    
    position_score <- if (best_result$brand_mentioned) {
      calculate_position_weight(
        first_position = best_result$first_position,
        text_length    = nchar(responses$responses[i]),
        mention_count  = best_result$mention_count
      )
    } else { 0 }
    
    presence_results <- presence_results %>%
      add_row(
        response_num      = i,
        response_text     = responses$responses[i],
        brand_mentioned   = best_result$brand_mentioned,
        first_position    = best_result$first_position,
        mention_count     = best_result$mention_count,
        position_score    = position_score,
        weighted_presence = position_score
      )
  }
  
  weighted_presence_score <- mean(presence_results$weighted_presence) * 100
  
  return(round(weighted_presence_score, 2))
}



# Main Interface Function -------------------------------------------------

#' Calculate presence from prompts (with LLM queries)
#' @param brand_name Character, name of brand
#' @return List with presence scores and details
calculate_presence_from_prompts <- function(brand_names,  # was brand_name
                                            input_data) {
  
  presence_responses <- input_data
  
  # Pass full vector to calculate_presence_from_responses
  presence_details <- calculate_presence_from_responses(brand_names,
                                                        presence_responses)
  
  return(presence_details)
}

