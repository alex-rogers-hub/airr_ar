# ============================================
# queries_send_return.R
# ============================================

all_queries <- function(brand_name,
                        industry       = NULL,
                        model          = "gpt-4o-mini",
                        temperature    = 0.3,
                        brand_reach    = "global",
                        reach_country  = NULL,
                        reach_region   = NULL,
                        reach_postcode = NULL,
                        use_batch      = TRUE) {
  
  reach_context <- build_reach_context(brand_reach, reach_country,
                                       reach_region, reach_postcode)
  reach_str     <- if (nzchar(reach_context)) paste0(" in ", reach_context) else ""
  near_me_str   <- build_near_me_suffix(brand_reach, reach_country, reach_postcode)
  
  has_industry  <- !is.null(industry) && nzchar(industry)
  industry_str  <- if (has_industry) industry else "their industry"
  
  reps <- 10
  
  prompts <- c(
    rep(paste0('Who are the top 10 competitors for the brand ', brand_name,
               reach_str,
               '? Return only the brand names separated by semi-colons and no other information'), reps),
    rep(paste0('In what industry is the brand ', brand_name, reach_str,
               '? Give your answer in the form "brand is in the __ industry", giving no other information'), reps),
    rep(paste0('Tell me about the brand ', brand_name, reach_str), reps),
    rep(paste0("When was ", brand_name, " founded? Give only the year, no other words."), reps),
    rep(paste0("Where is ", brand_name, " headquartered", reach_str,
               "? Give only the city and country."), reps),
    rep(paste0("Who is the current CEO of ", brand_name,
               "? Give only the name."), reps),
    rep(paste0("What industry is ", brand_name, " in", reach_str,
               "? Give a one-sentence answer."), reps),
    rep(paste0("Is ", brand_name, " owned by a parent company? ",
               "If so, which one? Give only the name of the company or the word 'none'"), reps),
    rep(paste0("What are the main products or services of ", brand_name,
               reach_str, "? List the top 3."), reps),
    rep(paste0("How would you describe ", brand_name, "'s pricing strategy",
               reach_str,
               ": budget, mid-range, or premium? Only respond with one of those three options"), reps),
    rep(paste0("Who is ", brand_name, "'s primary target market", reach_str, "?"), reps),
    rep(paste0("What was a major milestone in ", brand_name, "'s history?"), reps),
    rep(paste0("How did ", brand_name, " get started? Give a brief summary."), reps),
    rep(paste0("Approximately how many employees does ", brand_name,
               " have? Give the answer as a number only"), reps),
    rep(paste0("What is the approximate annual revenue of ", brand_name,
               "? Give the answer as a number of US dollars only"), reps),
    rep(paste0("In which countries or regions does ", brand_name, " operate",
               reach_str, "? List only the country names with no extra information"), reps),
    
    # Presence queries — industry based, not brand name
    rep(paste0('What are the best brands in the ', industry_str, ' industry',
               if (nzchar(near_me_str)) near_me_str else reach_str, '?'), reps),
    rep(paste0("What are the top brands in ", industry_str,
               if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
    rep(paste0("Which companies lead the ", industry_str, " industry",
               if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
    rep(paste0("Recommend some ", industry_str, " brands",
               if (nzchar(near_me_str)) near_me_str else reach_str), reps),
    rep(paste0("What are popular ", industry_str, " companies",
               if (nzchar(near_me_str)) near_me_str else reach_str, "?"), reps),
    rep(paste0("List major players in ", industry_str,
               if (nzchar(near_me_str)) near_me_str else reach_str), reps)
  )
  
  # Single API call — batch or async depending on context
  all_responses <- if (use_batch) {
    ask_chatgpt_batch(
      prompts       = prompts,
      model         = model,
      temperature   = temperature,
      poll_interval = 30,
      max_wait_mins = 120
    )
  } else {
    ask_chatgpt_async(
      prompts     = prompts,
      model       = model,
      temperature = temperature
    )
  }
  
  idx <- 0
  grab <- function(n) {
    result <- all_responses[(idx + 1):(idx + n)]
    idx <<- idx + n
    result
  }
  
  competitor_raw     <- grab(reps)
  industry_raw       <- grab(reps)
  description_raw    <- grab(reps)
  founded_raw        <- grab(reps)
  hq_raw             <- grab(reps)
  ceo_raw            <- grab(reps)
  industry_det_raw   <- grab(reps)
  parent_raw         <- grab(reps)
  products_raw       <- grab(reps)
  pricing_raw        <- grab(reps)
  target_raw         <- grab(reps)
  milestone_raw      <- grab(reps)
  origin_raw         <- grab(reps)
  employees_raw      <- grab(reps)
  revenue_raw        <- grab(reps)
  countries_raw      <- grab(reps)
  best_brands_raw    <- grab(reps)
  top_brands_raw     <- grab(reps)
  leading_brands_raw <- grab(reps)
  rec_brands_raw     <- grab(reps)
  popular_brands_raw <- grab(reps)
  major_players_raw  <- grab(reps)
  
  competitor_summary <- competitor_raw %>%
    unlist() %>%
    str_split(";") %>%
    unlist() %>%
    str_trim() %>%
    tibble(competitor = .) %>%
    filter(nchar(competitor) > 0) %>%
    count(competitor, sort = TRUE, name = "mentions") %>%
    head(9)
  
  industry_summary <- industry_raw %>%
    unlist() %>%
    tibble(industry = .) %>%
    mutate(industry_desc = substr(industry,
                                  12 + nchar(brand_name),
                                  nchar(industry) - 10)) %>%
    filter(nchar(industry_desc) > 0) %>%
    count(industry_desc, sort = TRUE, name = "mentions") %>%
    head(1)
  
  perception_data <- list(
    founded = list(
      data     = founded_raw,
      type     = "year",
      question = "When was the brand founded?"
    ),
    hq = list(
      data     = hq_raw,
      type     = "location",
      question = "Where is the brand headquartered?"
    ),
    ceo = list(
      data     = ceo_raw,
      type     = "name",
      question = "Who is the current CEO?"
    ),
    industry = list(
      data     = industry_det_raw,
      type     = "text",
      question = "What industry is the brand in?"
    ),
    parent_company = list(
      data     = parent_raw,
      type     = "categorical",
      question = "Parent company"
    ),
    products_services = list(
      data     = products_raw,
      type     = "list",
      question = "Main products/services"
    ),
    pricing = list(
      data     = pricing_raw,
      type     = "categorical",
      question = "Pricing strategy"
    ),
    target_market = list(
      data     = target_raw,
      type     = "text",
      question = "Primary target market"
    ),
    milestone = list(
      data     = milestone_raw,
      type     = "text",
      question = "Major milestone"
    ),
    start = list(
      data     = origin_raw,
      type     = "text",
      question = "How the brand started"
    ),
    num_employees = list(
      data     = employees_raw,
      type     = "numeric",
      question = "Number of employees"
    ),
    revenue = list(
      data     = revenue_raw,
      type     = "numeric",
      question = "Annual revenue"
    ),
    countries = list(
      data     = countries_raw,
      type     = "list",
      question = "Countries of operation"
    )
  )
  
  presence_data <- list(
    top_brands = list(
      data     = top_brands_raw,
      type     = "text",
      question = "Top brands in industry"
    ),
    leading_brands = list(
      data     = leading_brands_raw,
      type     = "text",
      question = "Leading companies in industry"
    ),
    rec_brands = list(
      data     = rec_brands_raw,
      type     = "text",
      question = "Recommended brands in industry"
    ),
    popular_companies = list(
      data     = popular_brands_raw,
      type     = "text",
      question = "Popular companies in industry"
    ),
    major_players = list(
      data     = major_players_raw,
      type     = "text",
      question = "Major players in industry"
    )
  )
  
  list(
    competitor_summary        = competitor_summary,
    industry_summary          = industry_summary,
    comp_second_response_list = best_brands_raw,
    description_list          = description_raw,
    presence_data             = presence_data,
    perception_data           = perception_data
  )
}


# ============================================
# prompt_queries — unchanged
# ============================================

prompt_queries <- function(prompt_input,
                           model       = "gpt-4o-mini",
                           temperature = 0.3,
                           use_batch   = FALSE) {
  
  comp_response_list <- if (use_batch) {
    ask_chatgpt_batch(
      prompts       = prompt_input,
      model         = model,
      temperature   = temperature,
      poll_interval = 30,
      max_wait_mins = 60
    )
  } else {
    ask_chatgpt_async(
      prompts     = prompt_input,
      model       = model,
      temperature = temperature
    )
  }
  
  resp_df <- data.frame(
    prompt    = prompt_input,
    responses = sanitise_text(unlist(comp_response_list)),
    stringsAsFactors = FALSE
  )
  
  return(resp_df)
}


# ============================================
# inject_near_me_location — unchanged
# ============================================

inject_near_me_location <- function(prompt_text,
                                    brand_reach    = "global",
                                    reach_country  = NULL,
                                    reach_region   = NULL,
                                    reach_postcode = NULL) {
  
  if (is.null(brand_reach) || brand_reach == "global") return(prompt_text)
  
  if (!is.null(reach_country)  && is.na(reach_country))  reach_country  <- NULL
  if (!is.null(reach_region)   && is.na(reach_region))   reach_region   <- NULL
  if (!is.null(reach_postcode) && is.na(reach_postcode)) reach_postcode <- NULL
  
  prefix <- switch(brand_reach,
                   "national" = {
                     if (!is.null(reach_country) && nzchar(reach_country)) {
                       paste0("In ", reach_country, ", ")
                     } else ""
                   },
                   "regional" = {
                     if (!is.null(reach_region) && nzchar(reach_region)) {
                       paste0("In ", reach_region, ", ")
                     } else if (!is.null(reach_country) && nzchar(reach_country)) {
                       paste0("In ", reach_country, ", ")
                     } else ""
                   },
                   "near_me" = {
                     near_str <- build_near_me_suffix("near_me", reach_country, reach_postcode)
                     if (nzchar(near_str)) {
                       return(gsub("near me", near_str, prompt_text, ignore.case = TRUE))
                     }
                     return(prompt_text)
                   },
                   ""
  )
  
  if (nzchar(prefix)) {
    first_char <- substr(prompt_text, 1, 1)
    rest       <- substr(prompt_text, 2, nchar(prompt_text))
    paste0(prefix, tolower(first_char), rest)
  } else {
    prompt_text
  }
}