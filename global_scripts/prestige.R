# prestige.R
# AiRR POV - Prestige Measurement
# Measures competitive positioning and authority
# 
# Formula: Prestige = (0.5 × CompetitiveRank) + (0.3 × AuthoritySignals) + (0.2 × CategoryLeadership)

library(stringr)
library(dplyr)
library(purrr)
library(tibble)
library(R6)

# Constants ---------------------------------------------------------------

PRESTIGE_WEIGHTS <- list(
  competitive_rank = 0.5,
  authority_signals = 0.3,
  category_leadership = 0.2
)


AUTHORITY_KEYWORDS_WEIGHTED <- list(
  tier_1 = list(  # Strongest signals (3 points each)
    keywords = c('leader', 'leading', 'best', 'top', '#1', 'number one', 'dominant'),
    weight = 3.0
  ),
  tier_2 = list(  # Strong signals (2 points each)
    keywords = c('recommended', 'premier', 'preferred', 'renowned', 'pioneering', 
                 'innovative', 'industry-standard', 'gold-standard'),
    weight = 2.0
  ),
  tier_3 = list(  # Moderate signals (1 point each)
    keywords = c('trusted', 'most popular', 'well-known', 'established', 'respected'),
    weight = 1.0
  )
)

LEADERSHIP_KEYWORDS_WEIGHTED <- list(
  tier_1 = list(  # Absolute dominance (3.0 points each)
    keywords = c(
      'market leader', 'industry leader', 'category leader',
      '#1', 'number one', 'number 1', 'ranked first',
      'dominant player', 'dominates', 'market dominance',
      'unrivaled', 'unmatched', 'undisputed leader'
    ),
    weight = 3.0
  ),
  
  tier_2 = list(  # Strong leadership (2.0 points each)
    keywords = c(
      'leading provider', 'leading brand', 'leads the market',
      'first choice', 'top choice', 'preferred choice',
      'go-to brand', 'go-to company', 'go-to choice',
      'largest', 'biggest', 'most recognized',
      'most popular', 'most trusted', 'most preferred',
      'benchmark', 'gold standard', 'industry standard',
      'sets the standard', 'pioneer', 'pioneering'
    ),
    weight = 2.0
  ),
  
  tier_3 = list(  # Competitive position (1.5 points each)
    keywords = c(
      'top brand', 'top company', 'top player',
      'among the best', 'one of the best', 'one of the top',
      'leading competitor', 'major player', 'key player',
      'well-established', 'household name',
      'widely regarded', 'highly regarded', 'widely recognized',
      'front-runner', 'frontrunner', 'at the forefront'
    ),
    weight = 1.5
  ),
  
  tier_4 = list(  # General prominence (1.0 point each)
    keywords = c(
      'popular', 'well-known', 'prominent',
      'established', 'reputable', 'respected',
      'successful', 'thriving', 'growing',
      'competitive', 'strong presence', 'significant player'
    ),
    weight = 1.0
  )
)


#' Find first mention position of a brand in text
#' @param text Character string to search
#' @param brand_patterns List of regex patterns for the brand
#' @return Integer position of first match, or NA if not found
find_first_mention <- function(text, brand_patterns) {
  if (is.null(brand_patterns)) return(NA_integer_)
  
  text_lower <- tolower(text)
  
  positions <- map_int(brand_patterns, function(pattern) {
    matches <- str_locate_all(text_lower, tolower(pattern))[[1]]
    if (nrow(matches) > 0) {
      return(matches[1, "start"])
    }
    return(NA_integer_)
  })
  
  # Return earliest position
  positions <- positions[!is.na(positions)]
  if (length(positions) == 0) return(NA_integer_)
  return(min(positions))
}

#' Find all mention positions of a brand in text
#' @param text Character string to search
#' @param brand_patterns List of regex patterns for the brand
#' @return List of position data frames (start, end)
find_all_mentions <- function(text, brand_patterns) {
  if (is.null(brand_patterns)) return(list())
  
  text_lower <- tolower(text)
  all_positions <- list()
  
  for (pattern in brand_patterns) {
    matches <- str_locate_all(text_lower, tolower(pattern))[[1]]
    if (nrow(matches) > 0) {
      all_positions <- c(all_positions, 
                         purrr::transpose(as.data.frame(matches)))
    }
  }
  
  return(all_positions)
}

#' Calculate competitive rank score from analyses
#' @param analyses Data frame with rank column
#' @param num_competitors Integer number of competitors
#' @return Numeric score 0-100
calculate_competitive_rank_score <- function(rank,
                                             num_competitors) {
  
  if(rank == 0){
    score <- 0
  } else {
    if(num_competitors > 0){
      score <- max(0, min(100, 100 * (1 - (rank - 1) / num_competitors)))
    } else {
      score <- 50  # Default
    }
  }
  return(score)
}

#' Calculate authority score with logarithmic scaling
#' @param authority_results Tibble with weighted authority scores per run
#' @return Numeric score 0-100
calculate_authority_score <- function(avg_weighted_score) {
  
  # Exponential curve with diminishing returns
  # 0 pts → 0
  # 3 pts → 55
  # 6 pts → 86
  # 10 pts → 96
  # 15+ pts → approaches 100
  
  score <- 100 * (1 - exp(-avg_weighted_score / 3))
  
  return(min(100, score))
}

#' Calculate category leadership score with smooth scaling
#' @param avg_weighted_score Tibble with weighted leadership scores per run
#' @return Numeric score 0-100
calculate_leadership_score <- function(avg_weighted_score) {
  
  # Exponential curve with diminishing returns
  # 0 pts → 0
  # 3 pts (some leadership mentions) → 55
  # 6 pts (consistent leadership language) → 86
  # 10 pts (strong dominance signals) → 96
  # 15+ pts → approaches 100
  
  score <- 100 * (1 - exp(-avg_weighted_score / 3))
  
  return(score)
}

#' Calculate overall prestige score
#' @param competitive_rank Numeric score 0-100
#' @param authority_signals Numeric score 0-100
#' @param category_leadership Numeric score 0-100
#' @param weights List of weights (default PRESTIGE_WEIGHTS)
#' @return Numeric overall prestige score
calculate_prestige_score <- function(competitive_rank,
                                     authority_signals, 
                                     category_leadership, 
                                     weights = PRESTIGE_WEIGHTS) {
  score <- (
    weights$competitive_rank * competitive_rank +
      weights$authority_signals * authority_signals +
      weights$category_leadership * category_leadership
  )
  return(score)
}


#' Calculate competitive rank based on mention order -------------
#' @param brand_name Name of the brand being scored
#' @param model Character, LLM model name
#' @param temperature Numeric, LLM temperature parameter
#' @return List with rank, mentioned_brands, and rank_order
prestige_queries <- function(brand_name,
                             model = "gpt-4o-mini",
                             temperature = 0.1) {
  
  # Initialize the return object
  prestige_query <- list()
  
  comp_response_list <- ask_chatgpt_async(
    prompts = c(rep(paste0('Who are the top 10 competitors for the brand ', 
                           brand_name, 
                           '? Return only the brand names separated by semi-colons and no other information'),10),
                rep(paste0('In what industry is the brand ', brand_name, 
                           '? Give your answer in the form "brand is in the __ industry", giving no other information'),10),
                rep(paste0('Tell me about the brand ', brand_name),10)),
    model = model,
    temperature = temperature
  )
  
  competitor_list <- comp_response_list[1:10]
  # Parse and count competitor mentions
  competitor_summary <- competitor_list %>%
    unlist() %>%
    str_split(";") %>%
    unlist() %>%
    str_trim() %>%
    tibble(competitor = .) %>%
    filter(nchar(competitor) > 0) %>%
    count(competitor, sort = TRUE, name = "mentions") %>%
    head(9)
  
  industry_list <- comp_response_list[11:20]
  
  industry_summary <- industry_list %>%
    unlist() %>%
    tibble(industry = .) %>%
    mutate(industry_desc = substr(industry, 12 + nchar(brand_name), nchar(industry) - 10)) %>%
    filter(nchar(industry_desc) > 0) %>%
    count(industry_desc, sort = TRUE, name = "mentions") %>%
    head(1)
  
  description_list <- comp_response_list[21:30]
  
  # second string of queries to start now --
  
  comp_second_response_list <- ask_chatgpt_async(
    prompts = c(rep(paste0('What are the best brands in the ', 
                           industry_summary$industry_desc[1], 
                           ' industry?'),10)),
    model = model,
    temperature = temperature
  )
  
  prestige_query$competitor_summary <- competitor_summary
  prestige_query$industry_summary <- industry_summary
  prestige_query$comp_second_response_list <- comp_second_response_list
  prestige_query$description_list <- description_list
  
  return(
    prestige_query
  )
}

#' Calculate Authority Signal values -------------
#' @param brand_name Name of the brand being scored
#' @param response_data list containing output data from LLM responses
#' @return List with authority score
calculate_auth_and_lead <- function(brand_name,
                                    response_data,
                                    # context_window = 75,
                                    weighted_keywords = AUTHORITY_KEYWORDS_WEIGHTED) {
  # removing context window as the whole text is regarding the brand
  resp_data <- tolower(response_data)
  
  # Initialize results storage
  authority_results <- tibble(
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
    authority_results <- authority_results %>%
      add_row(
        run = i,
        weighted_score = weighted_score,
        keyword_count = length(found_keywords),
        keywords_found = list(names(found_keywords))
      )
  }
  
  # Calculate final average
  auth_avg_weighted_score <- mean(authority_results$weighted_score)
  auth_avg_words_found <- mean(authority_results$keyword_count)
  
  # # Add summary row
  # authority_summary <- authority_results %>%
  #   summarise(
  #     total_runs = n(),
  #     avg_weighted_score = mean(weighted_score),
  #     min_score = min(weighted_score),
  #     max_score = max(weighted_score),
  #     sd_score = sd(weighted_score),
  #     total_unique_keywords = length(unique(unlist(keywords_found)))
  #   )
  
  leadership_results <- tibble(
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
    for (tier_name in names(LEADERSHIP_KEYWORDS_WEIGHTED)) {
      tier <- LEADERSHIP_KEYWORDS_WEIGHTED[[tier_name]]
      
      for (keyword in tier$keywords) {
        if (str_detect(tolower(resp_data_proc), fixed(tolower(keyword)))) {
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
    leadership_results <- leadership_results %>%
      add_row(
        run = i,
        weighted_score = weighted_score,
        keyword_count = length(found_keywords),
        keywords_found = list(names(found_keywords))
      )
  }
  
  # Calculate summary
  leadership_summary <- leadership_results %>%
    summarise(
      total_runs = n(),
      avg_weighted_score = mean(weighted_score),
      median_score = median(weighted_score),
      min_score = min(weighted_score),
      max_score = max(weighted_score),
      sd_score = sd(weighted_score)
    )
  
  # Calculate final average
  lead_avg_weighted_score <- mean(leadership_results$weighted_score)
  lead_avg_words_found <- mean(leadership_results$keyword_count)
  
  
  
  
  auth_lead_output <- list()
  auth_lead_output$auth_weighted_score <- auth_avg_weighted_score
  auth_lead_output$auth_count <- auth_avg_words_found
  auth_lead_output$auth_details <- authority_results
  auth_lead_output$lead_weighted_score <- lead_avg_weighted_score
  auth_lead_output$lead_count <- lead_avg_words_found
  auth_lead_output$lead_details <- leadership_results
  
  return(auth_lead_output)
}


#' Calculate competitive rank based on mention order -------------
#' @param brand_name Name of the brand being scored
#' @param response_data list containing output data from LLM responses
#' @return List with rank, mentioned_brands, and rank_order
calculate_competitive_rank <- function(brand_name,
                                       response_data) {
  
  resp_data <- response_data
  
  brands_to_compare <- c(brand_name, resp_data$competitor_summary$competitor)
  
  # Find first mention for each brand
  mention_results <- map_dfc(seq_along(resp_data$comp_second_response_list), function(i) {
    mentions <- map_int(brands_to_compare, function(brand) {
      find_first_mention(resp_data$comp_second_response_list[[i]], brand)
    })
    
    # Return as named vector (becomes a column)
    tibble(!!paste0("response_", i) := mentions)
  })
  
  # Add brand names as first column
  mention_results_tb <- tibble(brand = brands_to_compare) %>%
    bind_cols(mention_results) %>%
    mutate(
      across(
        starts_with("response_"),
        ~rank(.x, na.last = "keep", ties.method = "min"),
        .names = "rank_{str_extract(.col, '\\\\d+$')}"
      )
    ) %>%
    rowwise() %>%
    mutate(
      avg_rank = mean(c_across(starts_with("rank_")), na.rm = TRUE),
      times_ranked = sum(!is.na(c_across(starts_with("rank_"))))
    ) %>%
    ungroup() %>%
    arrange(avg_rank) %>%
    dplyr::filter(times_ranked > 5)
  
  # Find target brand rank
  sorted_brands <- mention_results_tb %>%
    select(brand, avg_rank)
  
  brand_rank <- sorted_brands %>%
    filter(brand == brand_name)
  
  if (nrow(brand_rank) == 0){
    return(list(
      rank = 0,
      mentioned_brands = c(sorted_brands),
      rank_order = c(sorted_brands)
    ))
  } else {
    return(list(
      rank = brand_rank %>% pull(avg_rank),
      mentioned_brands = c(sorted_brands),
      rank_order = c(sorted_brands)
    ))
  }
}

#' Calculate prestige score directly from prompt texts
#' @param brand_name Character, name of brand to score
#' @param brand_aliases Character vector of brand aliases
#' @param model Character, LLM model name
#' @param temperature Numeric, LLM temperature parameter
#' @return List with score, prompts, responses, and metadata
calculate_prestige_from_prompts <- function(brand_name,
                                            input_data,
                                            model = "gpt-4o-mini",
                                            temperature = 0.1) {
  
  # Create prompts dataframe
  prestige_responses <- input_data
  
  comp_rank <- calculate_competitive_rank(brand_name,
                                          prestige_responses[c("competitor_summary", "comp_second_response_list")])
  
  authority_leadership <- calculate_auth_and_lead(brand_name,
                                                  prestige_responses$description_list)
  
  comp_rank_score <- calculate_competitive_rank_score(comp_rank$rank,
                                                      length(comp_rank$mentioned_brands$brand))
  
  authority_score <- calculate_authority_score(authority_leadership$auth_weighted_score)
  
  leadership_score <- calculate_leadership_score(authority_leadership$lead_weighted_score)
  
  prestige_score <- calculate_prestige_score(comp_rank_score,
                                             authority_score,
                                             leadership_score,
                                             PRESTIGE_WEIGHTS)
  
  prestige_out <- list()
  
  prestige_out$prestige_score <- prestige_score
  prestige_out$prestige_rank_score <- comp_rank_score
  prestige_out$prestige_rank_comps <- comp_rank$mentioned_brands
  prestige_out$prestige_authority_score <- authority_score
  prestige_out$prestige_leadership_score <- leadership_score
  
  return(prestige_out)
}

#------------- TESTING -------------------

# Run prestige calculation 20 times and analyze consistency
test_prestige_stability <- function(brand_name, 
                                    n_runs = 20,
                                    model = "gpt-4o-mini",
                                    temperature = 0.1) {
  
  cat("Running prestige calculation", n_runs, "times for", brand_name, "...\n")
  
  # Initialize results storage
  stability_results <- tibble(
    run = integer(),
    prestige_score = numeric(),
    rank_score = numeric(),
    authority_score = numeric(),
    leadership_score = numeric(),
    num_competitors = integer(),
    run_timestamp = character()
  )
  
  # Store full outputs for detailed inspection
  full_outputs <- list()
  
  # Run the calculation n times
  for (i in 1:n_runs) {
    cat("  Run", i, "of", n_runs, "...\n")
    
    tryCatch({
      # Run the prestige calculation
      result <- calculate_prestige_from_prompts(
        brand_name = brand_name,
        model = model,
        temperature = temperature
      )
      
      # Extract key metrics
      stability_results <- stability_results %>%
        add_row(
          run = i,
          prestige_score = result$prestige_score,
          rank_score = result$prestige_rank_score,
          authority_score = result$prestige_authority_score,
          leadership_score = result$prestige_leadership_score,
          num_competitors = nrow(result$prestige_rank_comps),
          run_timestamp = as.character(Sys.time())
        )
      
      # Store full output
      full_outputs[[i]] <- result
      
    }, error = function(e) {
      warning(paste("Error in run", i, ":", e$message))
      stability_results <- stability_results %>%
        add_row(
          run = i,
          prestige_score = NA_real_,
          rank_score = NA_real_,
          authority_score = NA_real_,
          leadership_score = NA_real_,
          num_competitors = NA_integer_,
          run_timestamp = as.character(Sys.time())
        )
    })
    
    # Brief pause between runs to avoid rate limiting
    Sys.sleep(1)
  }
  
  cat("\nCalculating stability metrics...\n")
  
  # Calculate summary statistics
  summary_stats <- stability_results %>%
    summarise(
      across(
        c(prestige_score, rank_score, authority_score, leadership_score),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          cv = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100  # Coefficient of variation
        ),
        .names = "{.col}_{.fn}"
      ),
      successful_runs = sum(!is.na(prestige_score)),
      failed_runs = sum(is.na(prestige_score))
    )
  
  # Calculate confidence intervals (95%)
  confidence_intervals <- stability_results %>%
    summarise(
      across(
        c(prestige_score, rank_score, authority_score, leadership_score),
        list(
          ci_lower = ~mean(.x, na.rm = TRUE) - 1.96 * sd(.x, na.rm = TRUE),
          ci_upper = ~mean(.x, na.rm = TRUE) + 1.96 * sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )
  
  # Stability assessment
  stability_assessment <- tibble(
    metric = c("prestige_score", "rank_score", "authority_score", "leadership_score"),
    cv = c(
      summary_stats$prestige_score_cv,
      summary_stats$rank_score_cv,
      summary_stats$authority_score_cv,
      summary_stats$leadership_score_cv
    )
  ) %>%
    mutate(
      stability = case_when(
        cv < 5 ~ "Excellent (CV < 5%)",
        cv < 10 ~ "Good (CV < 10%)",
        cv < 15 ~ "Fair (CV < 15%)",
        cv < 20 ~ "Poor (CV < 20%)",
        TRUE ~ "Very Poor (CV >= 20%)"
      )
    )
  
  # Return comprehensive results
  list(
    brand_name = brand_name,
    runs = stability_results,
    summary = summary_stats,
    confidence_intervals = confidence_intervals,
    stability_assessment = stability_assessment,
    full_outputs = full_outputs,
    test_params = list(
      n_runs = n_runs,
      model = model,
      temperature = temperature
    )
  )
}
# 
# # Usage
# stability_test2 <- test_prestige_stability("Under Armour", n_runs = 20)
# 
# # View summary
# print(stability_test$summary)
# print(stability_test$stability_assessment)
# 
# 
# print(stability_test2$summary)
# print(stability_test2$stability_assessment)
# 
# # View all runs
# View(stability_test$runs)


