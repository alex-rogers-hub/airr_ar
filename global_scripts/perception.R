# perception.R
# AiRR POV - Perception Measurement
# Measures accuracy, sentiment, and attribute alignment
# 
# Formula: Perception = (0.4 × Accuracy) + (0.3 × Sentiment) + (0.3 × AttributeAlignment)

library(stringr)
library(dplyr)
library(purrr)
library(tibble)

# Constants ---------------------------------------------------------------

PERCEPTION_WEIGHTS <- list(
  # accuracy = 0.4,
  # sentiment = 0.3,
  # attribute_alignment = 0.3
  accuracy = 0.6,
  sentiment = 0.4
)

#' Measure consistency by data type
#' @param responses Character vector of responses
#' @param type Character, data type (year, location, name, text, categorical, list, numeric)
#' @param question Character, question text
#' @return List with consistency metrics
measure_consistency_by_type <- function(responses, type, question) {
  
  # Clean responses
  responses_clean <- responses[!is.na(responses) & nchar(responses) > 0]
  
  if (length(responses_clean) < 2) {
    return(list(
      score = 0,
      unique_count = length(responses_clean),
      most_common = NA_character_,
      agreement_rate = 0,
      details = "Insufficient responses"
    ))
  }
  
  # Apply type-specific measurement
  result <- switch(type,
                   "year" = measure_year_consistency(responses_clean),
                   "location" = measure_location_consistency(responses_clean),
                   "name" = measure_name_consistency(responses_clean),
                   "categorical" = measure_categorical_consistency(responses_clean),
                   "numeric" = measure_numeric_consistency(responses_clean),
                   "list" = measure_list_consistency(responses_clean),
                   "text" = measure_text_consistency(responses_clean)
  )
  
  return(result)
}

# Type-specific consistency measures --------------------------------------

#' Measure year consistency
measure_year_consistency <- function(responses) {
  # Extract 4-digit years
  years <- str_extract(responses, "\\b(19|20)\\d{2}\\b")
  years_clean <- years[!is.na(years)]
  
  if (length(years_clean) == 0) {
    return(list(score = 0, unique_count = 0, most_common = NA, agreement_rate = 0, details = "No years found"))
  }
  
  # Count frequency
  year_counts <- table(years_clean)
  most_common_year <- names(year_counts)[which.max(year_counts)]
  agreement_rate <- max(year_counts) / length(years_clean)
  
  # Score: perfect if all agree, decrease with more variation
  unique_years <- length(unique(years_clean))
  score <- if (unique_years == 1) {
    100
  } else if (unique_years == 2 && agreement_rate > 0.7) {
    80  # Likely a typo or minor error
  } else {
    max(0, 100 - (unique_years - 1) * 20)
  }
  
  list(
    score = score,
    unique_count = unique_years,
    most_common = most_common_year,
    agreement_rate = round(agreement_rate, 2),
    details = paste0(as.character(year_counts), collapse = ", ")
  )
}

#' Measure location consistency
measure_location_consistency <- function(responses) {
  # Normalize locations (lowercase, remove punctuation)
  locations <- responses %>%
    tolower() %>%
    str_remove_all("[.,;]") %>%
    str_trim()
  
  # Extract city names (usually first word/phrase)
  cities <- str_extract(locations, "^[a-z\\s]+")
  
  # Count frequency
  location_counts <- table(locations)
  most_common <- names(location_counts)[which.max(location_counts)]
  agreement_rate <- max(location_counts) / length(locations)
  
  # Score based on agreement
  score <- agreement_rate * 100
  
  list(
    score = round(score, 2),
    unique_count = length(unique(locations)),
    most_common = most_common,
    agreement_rate = round(agreement_rate, 2),
    details = paste(names(location_counts), "=", location_counts, collapse = "; ")
  )
}

#' Measure name consistency (for CEO, etc.)
measure_name_consistency <- function(responses) {
  # Normalize names
  names_clean <- responses %>%
    str_remove_all("[.,]") %>%
    str_trim() %>%
    tolower()
  
  # Extract last name (usually more stable)
  last_names <- str_extract(names_clean, "\\b[a-z]+$")
  
  # Count full name frequency
  name_counts <- table(names_clean)
  last_name_counts <- table(last_names)
  
  most_common_full <- names(name_counts)[which.max(name_counts)]
  most_common_last <- names(last_name_counts)[which.max(last_name_counts)]
  
  # Agreement on last name is most important
  last_name_agreement <- max(last_name_counts) / length(last_names[!is.na(last_names)])
  
  score <- last_name_agreement * 100
  
  list(
    score = round(score, 2),
    unique_count = length(unique(names_clean)),
    most_common = most_common_full,
    agreement_rate = round(last_name_agreement, 2),
    details = paste("Last name agreement on:", most_common_last)
  )
}

#' Measure categorical consistency (budget/mid-range/premium, etc.)
measure_categorical_consistency <- function(responses) {
  # Normalize
  categories <- responses %>%
    tolower() %>%
    str_trim() %>%
    str_remove_all("[.,;]")
  
  # Extract key category words
  category_map <- c(
    "budget" = "budget|low|cheap|affordable|economy",
    "mid-range" = "mid|middle|moderate|average",
    "premium" = "premium|high|luxury|expensive|upscale",
    "none" = "none|no|independent"
  )
  
  # Classify each response
  classified <- map_chr(categories, function(cat) {
    matches <- map_lgl(category_map, ~str_detect(cat, .x))
    if (any(matches)) {
      names(category_map)[which(matches)[1]]
    } else {
      "other"
    }
  })
  
  # Count
  category_counts <- table(classified)
  most_common <- names(category_counts)[which.max(category_counts)]
  agreement_rate <- max(category_counts) / length(classified)
  
  score <- agreement_rate * 100
  
  list(
    score = round(score, 2),
    unique_count = length(unique(classified)),
    most_common = most_common,
    agreement_rate = round(agreement_rate, 2),
    details = paste(names(category_counts), "=", category_counts, collapse = "; ")
  )
}

#' Measure numeric consistency (revenue, employees)
measure_numeric_consistency <- function(responses) {
  # Extract numbers (handling K, M, B suffixes)
  extract_number <- function(text) {
    # Remove currency symbols and commas
    text_clean <- str_remove_all(text, "[$,]")
    
    # Extract number and multiplier
    num_match <- str_extract(text_clean, "[0-9.]+\\s*[KMBkmb]?")
    
    if (is.na(num_match)) return(NA_real_)
    
    # Parse
    num_val <- as.numeric(str_extract(num_match, "[0-9.]+"))
    multiplier <- str_extract(num_match, "[KMBkmb]")
    
    if (!is.na(multiplier)) {
      mult_val <- case_when(
        toupper(multiplier) == "K" ~ 1000,
        toupper(multiplier) == "M" ~ 1000000,
        toupper(multiplier) == "B" ~ 1000000000,
        TRUE ~ 1
      )
      num_val <- num_val * mult_val
    }
    
    return(num_val)
  }
  
  numbers <- map_dbl(responses, extract_number)
  numbers_clean <- numbers[!is.na(numbers)]
  
  if (length(numbers_clean) < 2) {
    return(list(score = 0, unique_count = 0, most_common = NA, agreement_rate = 0, details = "No numbers found"))
  }
  
  # Calculate coefficient of variation
  mean_val <- mean(numbers_clean)
  sd_val <- sd(numbers_clean)
  cv <- sd_val / mean_val
  
  # Score: low CV = high consistency
  score <- max(0, min(100, 100 * (1 - cv)))
  
  # Format most common (median)
  median_val <- median(numbers_clean)
  most_common_formatted <- if (median_val >= 1e9) {
    paste0(round(median_val / 1e9, 1), "B")
  } else if (median_val >= 1e6) {
    paste0(round(median_val / 1e6, 1), "M")
  } else if (median_val >= 1e3) {
    paste0(round(median_val / 1e3, 1), "K")
  } else {
    as.character(round(median_val))
  }
  
  list(
    score = round(score, 2),
    unique_count = length(unique(numbers_clean)),
    most_common = most_common_formatted,
    agreement_rate = round(1 - min(1, cv), 2),
    details = paste0("CV=", round(cv, 2), ", Range: ", 
                     round(min(numbers_clean)), " to ", round(max(numbers_clean)))
  )
}

#' Measure list consistency (products, countries)
measure_list_consistency <- function(responses) {
  # Split into items
  all_items <- responses %>%
    str_split(",|;|\\n|and") %>%
    unlist() %>%
    str_trim() %>%
    tolower() %>%
    str_remove_all("^[0-9.]+\\s*") %>%  # Remove numbering
    .[nchar(.) > 0]
  
  # Count item frequency
  item_counts <- table(all_items)
  
  # Top items
  top_items <- sort(item_counts, decreasing = TRUE)[1:3]
  top_items_names <- names(top_items)
  
  # Calculate Jaccard similarity between all pairs
  response_lists <- map(responses, ~{
    str_split(.x, ",|;|\\n|and")[[1]] %>%
      str_trim() %>%
      tolower() %>%
      str_remove_all("^[0-9.]+\\s*") %>%
      .[nchar(.) > 0]
  })
  
  # Pairwise Jaccard
  n <- length(response_lists)
  similarities <- numeric()
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      intersection <- length(intersect(response_lists[[i]], response_lists[[j]]))
      union <- length(union(response_lists[[i]], response_lists[[j]]))
      if (union > 0) {
        similarities <- c(similarities, intersection / union)
      }
    }
  }
  
  avg_similarity <- if (length(similarities) > 0) mean(similarities) else 0
  score <- avg_similarity * 100
  
  list(
    score = round(score, 2),
    unique_count = length(unique(all_items)),
    most_common = paste(top_items_names, collapse = ", "),
    agreement_rate = round(avg_similarity, 2),
    details = paste0("Top: ", paste(names(top_items), "(", top_items, ")", collapse = ", "))
  )
}

#' Measure text consistency (narratives, descriptions)
measure_text_consistency <- function(responses) {
  # Tokenize
  tokenize <- function(text) {
    text %>%
      tolower() %>%
      str_remove_all("[^a-z0-9\\s]") %>%
      str_split("\\s+") %>%
      unlist() %>%
      .[nchar(.) > 3]  # Remove short words
  }
  
  tokens_list <- map(responses, tokenize)
  
  # Pairwise Jaccard similarity
  n <- length(tokens_list)
  similarities <- numeric()
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      intersection <- length(intersect(tokens_list[[i]], tokens_list[[j]]))
      union <- length(union(tokens_list[[i]], tokens_list[[j]]))
      if (union > 0) {
        similarities <- c(similarities, intersection / union)
      }
    }
  }
  
  avg_similarity <- if (length(similarities) > 0) mean(similarities) else 0
  score <- avg_similarity * 100
  
  # Find common themes
  all_tokens <- unlist(tokens_list)
  token_counts <- sort(table(all_tokens), decreasing = TRUE)
  common_themes <- paste(names(token_counts)[1:5], collapse = ", ")
  
  list(
    score = round(score, 2),
    unique_count = length(unique(all_tokens)),
    most_common = common_themes,
    agreement_rate = round(avg_similarity, 2),
    details = paste0("Avg similarity: ", round(avg_similarity, 2))
  )
}

measure_response_consistency <- function(responses) {
  
  response_groups <- responses
  
  # Measure consistency for each group
  results <- map_dfr(names(response_groups), function(group_name) {
    group <- response_groups[[group_name]]
    
    consistency_result <- measure_consistency_by_type(
      responses = group$data,
      type = group$type,
      question = group$question
    )
    
    tibble(
      question_group = group_name,
      question_text = group$question,
      data_type = group$type,
      consistency_score = consistency_result$score,
      unique_values = consistency_result$unique_count,
      most_common = consistency_result$most_common,
      agreement_rate = consistency_result$agreement_rate,
      details = list(consistency_result$details)
    )
  })
  
  # Calculate overall consistency
  overall_consistency <- mean(results$consistency_score, na.rm = TRUE)
  
  # Weight by importance (factual questions matter more)
  weighted_scores <- results %>%
    mutate(
      weight = case_when(
        question_group %in% c("founded", "hq", "ceo", "parent_company") ~ 1.5,  # High importance
        question_group %in% c("industry", "pricing", "products_services") ~ 1.2,  # Medium-high
        question_group %in% c("num_employees", "revenue", "countries") ~ 1.0,  # Medium
        TRUE ~ 0.8  # Lower importance for narrative questions
      ),
      weighted_score = consistency_score * weight
    )
  
  weighted_consistency <- sum(weighted_scores$weighted_score) / sum(weighted_scores$weight)
  
  consistency <- list(
    overall_consistency = round(overall_consistency, 2),
    weighted_consistency = round(weighted_consistency, 2),
    by_question = results,
    interpretation = case_when(
      weighted_consistency >= 85 ~ "Excellent - Highly consistent factual information",
      weighted_consistency >= 70 ~ "Good - Mostly consistent with minor variations",
      weighted_consistency >= 55 ~ "Fair - Moderate consistency, some disagreement",
      weighted_consistency >= 40 ~ "Poor - Significant inconsistencies",
      TRUE ~ "Very Poor - Highly unreliable information"
    )
  )
  
  return(consistency)
}


#-------------- SENTIMENT STUFF -----------------
SENTIMENT_KEYWORDS_WEIGHTED <- list(
  
  very_positive = list(
    keywords = c(
      'excellent', 'outstanding', 'exceptional', 'amazing', 'fantastic',
      'superior', 'best', 'leading', 'premier', 'iconic',
      'revolutionary', 'groundbreaking', 'world-class', 'unmatched'
    ),
    weight = 2.0
  ),
  
  positive = list(
    keywords = c(
      'good', 'great', 'quality', 'reliable', 'trusted',
      'innovative', 'popular', 'respected', 'successful', 'impressive',
      'recommended', 'preferred', 'reputable', 'strong', 'solid'
    ),
    weight = 1.0
  ),
  
  negative = list(
    keywords = c(
      'poor', 'bad', 'disappointing', 'mediocre', 'overpriced',
      'expensive', 'unreliable', 'problematic', 'concerns', 'issues',
      'criticized', 'controversial', 'complaints', 'struggling', 'declining'
    ),
    weight = -1.0
  ),
  
  very_negative = list(
    keywords = c(
      'terrible', 'worst', 'awful', 'horrible', 'failing',
      'scandal', 'lawsuit', 'fraud', 'unethical', 'dangerous',
      'bankrupt', 'collapse', 'disaster', 'catastrophic'
    ),
    weight = -2.0
  )
)

#' Analyze sentiment with weighted keywords
#' @param response_data Character, text to analyze
#' @param weighted_keywords Character, words to check for
#' @return List with sentiment score and details
analyze_sentiment_weighted <- function(response_data,
                                       weighted_keywords = SENTIMENT_KEYWORDS_WEIGHTED) {
  
  resp_data <- tolower(response_data)
  
  # Initialize results storage
  sentiment_results <- tibble(
    run = integer(),
    weighted_score = numeric(),
    keyword_count = integer(),
    keywords_found = list()
  )
  
  # Run analysis 10 times
  for(i in 1:10){
    resp_data_proc <- resp_data[i]
    
    found_keywords <- list()
    
    # Check each tier
    for (tier_name in names(weighted_keywords)) {
      tier <- weighted_keywords[[tier_name]]
      
      for (keyword in tier$keywords) {
        if (str_detect(resp_data_proc, fixed(keyword))) {
          # Store keyword with its weight (avoid duplicates)
          if (!keyword %in% names(found_keywords)) {
            found_keywords[[keyword]] <- tier$weight
          }
        }
      }
    }
    
    # Calculate weighted score for this run
    weighted_score <- sum(unlist(found_keywords))
    
    # Store results
    sentiment_results <- sentiment_results %>%
      add_row(
        run = i,
        weighted_score = weighted_score,
        keyword_count = length(found_keywords),
        keywords_found = list(names(found_keywords))
      )
  }
  
  # Calculate final average
  sent_avg_weighted_score <- mean(sentiment_results$weighted_score)
  sent_avg_words_found <- mean(sentiment_results$keyword_count)
  
  
  # Normalize to 0-100 scale
  # Score range: typically -10 to +10, map to 0-100
  normalized_score <- (sent_avg_weighted_score + 10) / 20 * 100
  normalized_score <- max(0, min(100, normalized_score))
  
  # Classify sentiment
  sentiment_label <- case_when(
    normalized_score >= 70 ~ "Very Positive",
    normalized_score >= 55 ~ "Positive",
    normalized_score >= 45 ~ "Neutral",
    normalized_score >= 30 ~ "Negative",
    TRUE ~ "Very Negative"
  )
  
  sentiment_results <- list(
    score = round(normalized_score, 2),
    raw_score = sent_avg_weighted_score,
    label = sentiment_label,
    keywords_found = names(found_keywords),
    keyword_weights = unlist(found_keywords),
    positive_count = sum(unlist(found_keywords) > 0),
    negative_count = sum(unlist(found_keywords) < 0)
  )
  
  return(sentiment_results)
}


#' Calculate perception score directly from prompt texts
#' @param brand_name Character, name of brand to score
#' @param model Character, LLM model name
#' @param temperature Numeric, LLM temperature parameter
#' @return List with score, prompts, responses, and metadata
calculate_perception_from_prompts <- function(brand_name,
                                              input_data,
                                              model = "gpt-4o-mini",
                                              temperature = 0.1) {
  
  # Create responses dataframe
  all_responses <- input_data
  
  perception_responses <- all_responses$perception_data
  prestige_responses <- all_responses$description_list
  
  accuracy_score <- measure_response_consistency(perception_responses)
  
  sentiment_score <- analyze_sentiment_weighted(prestige_responses)
  
  perception_score <- (PERCEPTION_WEIGHTS$accuracy*accuracy_score$weighted_consistency) +
    (PERCEPTION_WEIGHTS$sentiment*sentiment_score$score)
  
  perception_out <- list()
  
  perception_out$perception_score <- perception_score
  perception_out$perception_accuracy_score <- accuracy_score$weighted_consistency
  perception_out$perception_sentiment_score <- sentiment_score$score
  perception_out$prestige_accuracy_interpretation <- accuracy_score$interpretation
  
  return(perception_out)
}



# Test Perception Stability -----------------------------------------------

#' #' Run perception calculation multiple times and analyze stability
#' #' @param brand_name Character, name of brand
#' #' @param n_runs Integer, number of test runs
#' #' @param model Character, LLM model
#' #' @param temperature Numeric, LLM temperature
#' #' @return List with stability analysis
#' test_perception_stability <- function(brand_name,
#'                                       n_runs = 20,
#'                                       model = "gpt-4o-mini",
#'                                       temperature = 0.1) {
#'   
#'   cat("\n")
#'   cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
#'   cat("PERCEPTION SCORE STABILITY TEST\n")
#'   cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")
#'   
#'   cat("Brand:", brand_name, "\n")
#'   cat("Runs:", n_runs, "\n")
#'   cat("Model:", model, "\n")
#'   cat("Temperature:", temperature, "\n\n")
#'   
#'   # Initialize results storage
#'   stability_results <- tibble(
#'     run = integer(),
#'     perception_score = numeric(),
#'     accuracy_score = numeric(),
#'     sentiment_score = numeric(),
#'     accuracy_interpretation = character(),
#'     run_timestamp = character()
#'   )
#'   
#'   # Store full outputs for detailed inspection
#'   full_outputs <- list()
#'   
#'   # Store detailed accuracy results
#'   accuracy_details <- list()
#'   
#'   # Run the calculation n times
#'   for (i in 1:n_runs) {
#'     cat("Run", i, "of", n_runs, "...")
#'     
#'     tryCatch({
#'       start_time <- Sys.time()
#'       
#'       # Run the perception calculation
#'       result <- calculate_perception_from_prompts(
#'         brand_name = brand_name,
#'         model = model,
#'         temperature = temperature
#'       )
#'       
#'       end_time <- Sys.time()
#'       elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
#'       
#'       cat(" Complete (", round(elapsed, 1), "s)\n", sep = "")
#'       
#'       # Extract key metrics
#'       stability_results <- stability_results %>%
#'         add_row(
#'           run = i,
#'           perception_score = result$perception_score,
#'           accuracy_score = result$perception_accuracy_score,
#'           sentiment_score = result$perception_sentiment_score,
#'           accuracy_interpretation = result$prestige_accuracy_interpretation,
#'           run_timestamp = as.character(Sys.time())
#'         )
#'       
#'       # Store full output
#'       full_outputs[[i]] <- result
#'       
#'     }, error = function(e) {
#'       cat(" ERROR:", e$message, "\n")
#'       warning(paste("Error in run", i, ":", e$message))
#'       
#'       stability_results <- stability_results %>%
#'         add_row(
#'           run = i,
#'           perception_score = NA_real_,
#'           accuracy_score = NA_real_,
#'           sentiment_score = NA_real_,
#'           accuracy_interpretation = "Error",
#'           run_timestamp = as.character(Sys.time())
#'         )
#'     })
#'     
#'     # Brief pause between runs
#'     if (i < n_runs) Sys.sleep(2)
#'   }
#'   
#'   cat("\nCalculating stability metrics...\n")
#'   
#'   # Calculate summary statistics
#'   summary_stats <- stability_results %>%
#'     summarise(
#'       across(
#'         c(perception_score, accuracy_score, sentiment_score),
#'         list(
#'           mean = ~mean(.x, na.rm = TRUE),
#'           median = ~median(.x, na.rm = TRUE),
#'           sd = ~sd(.x, na.rm = TRUE),
#'           min = ~min(.x, na.rm = TRUE),
#'           max = ~max(.x, na.rm = TRUE),
#'           cv = ~(sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100
#'         ),
#'         .names = "{.col}_{.fn}"
#'       ),
#'       successful_runs = sum(!is.na(perception_score)),
#'       failed_runs = sum(is.na(perception_score))
#'     )
#'   
#'   # Calculate confidence intervals (95%)
#'   confidence_intervals <- stability_results %>%
#'     summarise(
#'       across(
#'         c(perception_score, accuracy_score, sentiment_score),
#'         list(
#'           ci_lower = ~mean(.x, na.rm = TRUE) - 1.96 * sd(.x, na.rm = TRUE),
#'           ci_upper = ~mean(.x, na.rm = TRUE) + 1.96 * sd(.x, na.rm = TRUE)
#'         ),
#'         .names = "{.col}_{.fn}"
#'       )
#'     )
#'   
#'   # Stability assessment
#'   stability_assessment <- tibble(
#'     metric = c("perception_score", "accuracy_score", "sentiment_score"),
#'     cv = c(
#'       summary_stats$perception_score_cv,
#'       summary_stats$accuracy_score_cv,
#'       summary_stats$sentiment_score_cv
#'     )
#'   ) %>%
#'     mutate(
#'       stability = case_when(
#'         cv < 5 ~ "Excellent (CV < 5%)",
#'         cv < 10 ~ "Good (CV < 10%)",
#'         cv < 15 ~ "Fair (CV < 15%)",
#'         cv < 20 ~ "Poor (CV < 20%)",
#'         TRUE ~ "Very Poor (CV >= 20%)"
#'       )
#'     )
#'   
#'   # Accuracy interpretation frequency
#'   interpretation_freq <- stability_results %>%
#'     count(accuracy_interpretation, sort = TRUE) %>%
#'     mutate(percentage = n / sum(n) * 100)
#'   
#'   # Return comprehensive results
#'   list(
#'     brand_name = brand_name,
#'     runs = stability_results,
#'     summary = summary_stats,
#'     confidence_intervals = confidence_intervals,
#'     stability_assessment = stability_assessment,
#'     interpretation_frequency = interpretation_freq,
#'     full_outputs = full_outputs,
#'     test_params = list(
#'       n_runs = n_runs,
#'       model = model,
#'       temperature = temperature
#'     )
#'   )
#' }
#' 
#' ttt <- test_perception_stability('Nike')
#' print(ttt$summary)
#' print(ttt$stability_assessment)
#' 
#' ttt2 <- test_perception_stability('Under Armour')
#' print(ttt2$summary)
#' print(ttt2$stability_assessment)

