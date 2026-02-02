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
      first_position = NA_integer_,
      mention_count = 0,
      all_positions = integer(0)
    ))
  }
  
  text_lower <- tolower(text)
  all_positions <- integer(0)
  
  # Find all matches for all patterns
  for (pattern in brand_name) {
    matches <- str_locate_all(text_lower, regex(tolower(pattern), ignore_case = TRUE))[[1]]
    
    if (nrow(matches) > 0) {
      all_positions <- c(all_positions, matches[, "start"])
    }
  }
  
  # Remove duplicates and sort
  all_positions <- unique(all_positions)
  all_positions <- sort(all_positions)
  
  list(
    brand_mentioned = length(all_positions) > 0,
    first_position = if (length(all_positions) > 0) all_positions[1] else NA_integer_,
    mention_count = length(all_positions),
    all_positions = all_positions
  )
}

#' Calculate presence score from response groups with position weighting
#' @param brand_name Character, name of brand to search for
#' @param brand_aliases Character vector, alternative names for brand
#' @param presence_responses List of response groups from all_queries
#' @return List with presence score and details
calculate_presence_from_responses <- function(brand_name,
                                              presence_responses) {
  
  # Compile brand patterns
  all_brand_names <- c(brand_name, brand_aliases)
  brand_patterns <- map(all_brand_names, ~paste0("\\b", escape_regex(.x), "\\b"))
  
  # Initialize results storage
  presence_results <- tibble(
    group_name = character(),
    response_num = integer(),
    response_text = character(),
    brand_mentioned = logical(),
    first_position = integer(),
    mention_count = integer(),
    position_score = numeric(),
    weighted_presence = numeric()
  )
  
  # Analyze each response group
  for (group_name in names(presence_responses)) {
    group_data <- presence_responses[[group_name]]$data
    
    for (i in seq_along(group_data)) {
      response <- group_data[i]
      
      if (is.na(response) || nchar(response) == 0) {
        presence_results <- presence_results %>%
          add_row(
            group_name = group_name,
            response_num = i,
            response_text = response,
            brand_mentioned = FALSE,
            first_position = NA_integer_,
            mention_count = 0,
            position_score = 0,
            weighted_presence = 0
          )
        next
      }
      
      # Detect mentions
      mention_result <- detect_brand_with_position(response, brand_name)
      
      # Calculate position-based score
      if (mention_result$brand_mentioned) {
        position_score <- calculate_position_weight(
          first_position = mention_result$first_position,
          text_length = nchar(response),
          mention_count = mention_result$mention_count
        )
      } else {
        position_score <- 0
      }
      
      # Store results
      presence_results <- presence_results %>%
        add_row(
          group_name = group_name,
          response_num = i,
          response_text = unlist(response),
          brand_mentioned = mention_result$brand_mentioned,
          first_position = mention_result$first_position,
          mention_count = mention_result$mention_count,
          position_score = position_score,
          weighted_presence = position_score
        )
    }
  }
  
  # Calculate overall presence score
  total_responses <- nrow(presence_results)
  
  # Method 1: Simple mention rate
  simple_mention_rate <- mean(presence_results$brand_mentioned) * 100
  
  # Method 2: Position-weighted presence
  weighted_presence_score <- mean(presence_results$weighted_presence) * 100
  
  # Method 3: By question type
  by_question <- presence_results %>%
    group_by(group_name) %>%
    summarise(
      mention_rate = mean(brand_mentioned),
      avg_position_score = mean(position_score),
      avg_first_position = mean(first_position, na.rm = TRUE),
      total_mentions = sum(mention_count),
      .groups = "drop"
    ) %>%
    arrange(desc(mention_rate))
  
  # Calculate confidence/stability
  mention_variance <- sd(presence_results$weighted_presence)
  
  # Interpretation
  interpretation <- case_when(
    weighted_presence_score >= 80 ~ "Excellent - Strong, prominent presence",
    weighted_presence_score >= 65 ~ "Very Good - Consistent presence with good positioning",
    weighted_presence_score >= 50 ~ "Good - Regular presence",
    weighted_presence_score >= 35 ~ "Fair - Moderate presence",
    weighted_presence_score >= 20 ~ "Weak - Infrequent presence",
    TRUE ~ "Very Weak - Minimal presence"
  )
  
  # Return comprehensive results
  presence_details <- list(
    brand_name = brand_name,
    overall_score = round(weighted_presence_score, 2),
    simple_mention_rate = round(simple_mention_rate, 2),
    total_responses = total_responses,
    responses_with_mentions = sum(presence_results$brand_mentioned),
    by_question = by_question,
    detailed_results = presence_results,
    interpretation = interpretation,
    stability = round(100 - min(100, mention_variance * 100), 2),
    timestamp = Sys.time()
  )
  
  return(presence_details)
}

#' Calculate presence score from response groups with position weighting
#' @param brand_name Character, name of brand to search for
#' @param brand_aliases Character vector, alternative names for brand
#' @param presence_responses List of response groups from all_queries
#' @return List with presence score and details
presence_prompt_calc <- function(brand_name,
                                 presence_responses) {
  

  responses <- presence_responses
  
  # Initialize results storage
  presence_results <- tibble(
    response_num = integer(),
    response_text = character(),
    brand_mentioned = logical(),
    first_position = integer(),
    mention_count = integer(),
    position_score = numeric(),
    weighted_presence = numeric()
  )
  
  for(i in 1:nrow(responses)){
    mention_result <- detect_brand_with_position(responses$responses[i], brand_name)
    
    # Calculate position-based score
    if (mention_result$brand_mentioned) {
      position_score <- calculate_position_weight(
        first_position = mention_result$first_position,
        text_length = nchar(responses$responses[i]),
        mention_count = mention_result$mention_count
      )
    } else {
      position_score <- 0
    }
    
    # Store results
    presence_results <- presence_results %>%
      add_row(
        response_num = i,
        response_text = responses$responses[i],
        brand_mentioned = mention_result$brand_mentioned,
        first_position = mention_result$first_position,
        mention_count = mention_result$mention_count,
        position_score = position_score,
        weighted_presence = position_score
      )
    
  }
  
  # Calculate overall presence score
  total_responses <- nrow(presence_results)
  
  # Method 1: Simple mention rate
  simple_mention_rate <- mean(presence_results$brand_mentioned) * 100
  
  # Method 2: Position-weighted presence
  weighted_presence_score <- mean(presence_results$weighted_presence) * 100
  
  # Method 3: By question type
  by_question <- presence_results %>%
    summarise(
      mention_rate = mean(brand_mentioned),
      avg_position_score = mean(position_score),
      avg_first_position = mean(first_position, na.rm = TRUE),
      total_mentions = sum(mention_count),
      .groups = "drop"
    ) %>%
    arrange(desc(mention_rate))
  
  # Calculate confidence/stability
  mention_variance <- sd(presence_results$weighted_presence)
  
  # Interpretation
  interpretation <- case_when(
    weighted_presence_score >= 80 ~ "Excellent - Strong, prominent presence",
    weighted_presence_score >= 65 ~ "Very Good - Consistent presence with good positioning",
    weighted_presence_score >= 50 ~ "Good - Regular presence",
    weighted_presence_score >= 35 ~ "Fair - Moderate presence",
    weighted_presence_score >= 20 ~ "Weak - Infrequent presence",
    TRUE ~ "Very Weak - Minimal presence"
  )
  
  # Return comprehensive results
  presence_details <- list(
    brand_name = brand_name,
    overall_score = round(weighted_presence_score, 2),
    simple_mention_rate = round(simple_mention_rate, 2),
    total_responses = total_responses,
    responses_with_mentions = sum(presence_results$brand_mentioned),
    by_question = by_question,
    detailed_results = presence_results,
    interpretation = interpretation,
    stability = round(100 - min(100, mention_variance * 100), 2),
    timestamp = Sys.time()
  )
  
  return(presence_details$overall_score)
}



# Main Interface Function -------------------------------------------------

#' Calculate presence from prompts (with LLM queries)
#' @param brand_name Character, name of brand
#' @return List with presence scores and details
calculate_presence_from_prompts <- function(brand_name,
                                            input_data) {
  
  # Create responses dataframe
  # presence_responses <- all_queries(brand_name)$presence_data
  
  presence_responses <- input_data
  
  presence_details <- calculate_presence_from_responses(brand_name,
                                                        presence_responses)
  
  return(presence_details)
}

