

#' Calculate competitive rank based on mention order -------------
#' @param brand_name Name of the brand being scored
#' @param model Character, LLM model name
#' @param temperature Numeric, LLM temperature parameter
#' @return List with rank, mentioned_brands, and rank_order
all_queries <- function(brand_name,
                             model = "gpt-4o-mini",
                             temperature = 0.1) {
  
  reps <- 10
  
  comp_response_list <- ask_chatgpt_async(
    prompts = c(rep(paste0('Who are the top 10 competitors for the brand ', 
                           brand_name, 
                           '? Return only the brand names separated by semi-colons and no other information'),10),
                rep(paste0('In what industry is the brand ', brand_name, 
                           '? Give your answer in the form "brand is in the __ industry", giving no other information'),10),
                rep(paste0('Tell me about the brand ', brand_name),10),
                
                # Basic Facts
                rep(paste0("When was ", brand_name, " founded? Give only the year, no other words."), 10),
                rep(paste0("Where is ", brand_name, " headquartered? Give only the city and country."), 10),
                rep(paste0("Who is the current CEO of ", brand_name, "? Give only the name."), 10),
                rep(paste0("What industry is ", brand_name, " in? Give a one-sentence answer."), 10),
                rep(paste0("Is ", brand_name, " owned by a parent company? If so, which one? Give only the name of the company or the word 'none'"), 10),
                
                # Product/Service Facts
                rep(paste0("What are the main products or services of ", brand_name, "? List the top 3."), 10),
                rep(paste0("How would you describe ", brand_name, "'s pricing strategy: budget, mid-range, or premium? Only respond with one of those three options"), 10),
                rep(paste0("Who is ", brand_name, "'s primary target market?"), 10),
                
                # Historical Facts
                rep(paste0("What was a major milestone in ", brand_name, "'s history?"), 10),
                rep(paste0("How did ", brand_name, " get started? Give a brief summary."), 10),
                
                # Company Size
                rep(paste0("Approximately how many employees does ", brand_name, " have? Give the answer as a number only"), 10),
                rep(paste0("What is the approximate annual revenue of ", brand_name, "? Give the answer as a number of US dollars only"), 10),
                rep(paste0("In which countries or regions does ", brand_name, " operate? List only the country names with no extra information"), 10)),
    model = model,
    temperature = temperature
  )
  
  # Split responses into groups
  response_groups1 <- list(
    competitor_list = list(
      data = comp_response_list[1:reps],
      type = "text",
      question = "Who are the top 10 competitors?"
    ),
    industry_list = list(
      data = comp_response_list[(reps+1):(2*reps)],
      type = "text",
      question = "In what industry is the brand?"
    ),
    description_list = list(
      data = comp_response_list[(2*reps+1):(3*reps)],
      type = "text",
      question = "Tell me about the brand?"
    ),
    founded = list(
      data = comp_response_list[(3*reps+1):(4*reps)],
      type = "year",
      question = "When was the brand founded?"
    ),
    hq = list(
      data = comp_response_list[(4*reps+1):(5*reps)],
      type = "location",
      question = "Where is the brand headquartered?"
    ),
    ceo = list(
      data = comp_response_list[(5*reps+1):(6*reps)],
      type = "name",
      question = "Who is the current CEO?"
    ),
    industry = list(
      data = comp_response_list[(6*reps+1):(7*reps)],
      type = "text",
      question = "What industry is the brand in?"
    ),
    parent_company = list(
      data = comp_response_list[(7*reps+1):(8*reps)],
      type = "categorical",
      question = "Parent company"
    ),
    products_services = list(
      data = comp_response_list[(8*reps+1):(9*reps)],
      type = "list",
      question = "Main products/services"
    ),
    pricing = list(
      data = comp_response_list[(9*reps+1):(10*reps)],
      type = "categorical",
      question = "Pricing strategy"
    ),
    target_market = list(
      data = comp_response_list[(10*reps+1):(11*reps)],
      type = "text",
      question = "Primary target market"
    ),
    milestone = list(
      data = comp_response_list[(11*reps+1):(12*reps)],
      type = "text",
      question = "Major milestone"
    ),
    start = list(
      data = comp_response_list[(12*reps+1):(13*reps)],
      type = "text",
      question = "How the brand started"
    ),
    num_employees = list(
      data = comp_response_list[(13*reps+1):(14*reps)],
      type = "numeric",
      question = "Number of employees"
    ),
    revenue = list(
      data = comp_response_list[(14*reps+1):(15*reps)],
      type = "numeric",
      question = "Annual revenue"
    ),
    countries = list(
      data = comp_response_list[(15*reps+1):(16*reps)],
      type = "list",
      question = "Countries of operation"
    )
  )
  
  # Parse and count competitor mentions
  competitor_summary <- response_groups1$competitor_list$data %>%
    unlist() %>%
    str_split(";") %>%
    unlist() %>%
    str_trim() %>%
    tibble(competitor = .) %>%
    filter(nchar(competitor) > 0) %>%
    count(competitor, sort = TRUE, name = "mentions") %>%
    head(9)
  
  industry_summary <- response_groups1$industry_list$data %>%
    unlist() %>%
    tibble(industry = .) %>%
    mutate(industry_desc = substr(industry, 12 + nchar(brand_name), nchar(industry) - 10)) %>%
    filter(nchar(industry_desc) > 0) %>%
    count(industry_desc, sort = TRUE, name = "mentions") %>%
    head(1)
  
  # second string of queries to start now --
  
  industry <- industry_summary$industry_desc[1]
  
  comp_second_response_list <- ask_chatgpt_async(
    prompts = c(rep(paste0('What are the best brands in the ', industry, ' industry?'),10),
                rep(paste0("What are the top brands in ", industry, "?"),10),
                rep(paste0("Which companies lead the ", industry, " industry?"),10),
                rep(paste0("Recommend some ", industry, " brands"),10),
                rep(paste0("What are popular ", industry, " companies?"),10),
                rep(paste0("List major players in ", industry),10)
    ),
    model = model,
    temperature = temperature
  )
  
  # Split responses into groups
  response_groups2 <- list(
    industry_brands = list(
      data = comp_second_response_list[1:reps],
      type = "text",
      question = "What are the best brands in the industry?"
    ),
    industry_brands2 = list(
      data = comp_second_response_list[(reps+1):(2*reps)],
      type = "text",
      question = "What are the top brands in the industry?"
    ),
    leading_brands = list(
      data = comp_second_response_list[(2*reps+1):(3*reps)],
      type = "text",
      question = "Which companies lead the industry?"
    ),
    rec_brands = list(
      data = comp_second_response_list[(3*reps+1):(4*reps)],
      type = "text",
      question = "Recommend brands in industry?"
    ),
    popular_companies = list(
      data = comp_second_response_list[(4*reps+1):(5*reps)],
      type = "text",
      question = "Popular companies in industry"
    ),
    major_players = list(
      data = comp_second_response_list[(5*reps+1):(6*reps)],
      type = "text",
      question = "Major players in industry"
    )
  )
  
  # Initialize the return object
  all_query <- list()
  
  all_query$competitor_summary <- competitor_summary
  all_query$industry_summary <- industry_summary
  all_query$comp_second_response_list <- response_groups2$industry_brands$data
  all_query$description_list <- response_groups1$description_list$data
  all_query$presence_data <- response_groups2[2:6]
  all_query$perception_data <- response_groups1[4:16]
  
  return(
    all_query
  )
}


#' Calculate competitive rank based on mention order -------------
#' @param brand_name Name of the brand being scored
#' @param model Character, LLM model name
#' @param temperature Numeric, LLM temperature parameter
#' @return List with rank, mentioned_brands, and rank_order
perception_queries <- function(brand_name,
                               model = "gpt-4o-mini",
                               temperature = 0.1) {
  
  # Initialize the return object
  perception_query <- list()
  
  comp_response_list <- ask_chatgpt_async(
    prompts = c(
      # Basic Facts
      rep(paste0("When was ", brand_name, " founded? Give only the year, no other words."), 10),
      rep(paste0("Where is ", brand_name, " headquartered? Give only the city and country."), 10),
      rep(paste0("Who is the current CEO of ", brand_name, "? Give only the name."), 10),
      rep(paste0("What industry is ", brand_name, " in? Give a one-sentence answer."), 10),
      rep(paste0("Is ", brand_name, " owned by a parent company? If so, which one? Give only the name of the company or the word 'none'"), 10),
      
      # Product/Service Facts
      rep(paste0("What are the main products or services of ", brand_name, "? List the top 3."), 10),
      rep(paste0("How would you describe ", brand_name, "'s pricing strategy: budget, mid-range, or premium? Only respond with one of those three options"), 10),
      rep(paste0("Who is ", brand_name, "'s primary target market?"), 10),
      
      # Historical Facts
      rep(paste0("What was a major milestone in ", brand_name, "'s history?"), 10),
      rep(paste0("How did ", brand_name, " get started? Give a brief summary."), 10),
      
      # Company Size
      rep(paste0("Approximately how many employees does ", brand_name, " have? Give the answer as a number only"), 10),
      rep(paste0("What is the approximate annual revenue of ", brand_name, "? Give the answer as a number of US dollars only"), 10),
      rep(paste0("In which countries or regions does ", brand_name, " operate? List only the country names with no extra information"), 10)
    ),
    model = model,
    temperature = temperature
  )
  
  return(
    comp_response_list
  )
}

