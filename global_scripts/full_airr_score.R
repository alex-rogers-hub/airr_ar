full_air_score <- function(brand_name,
                           model          = "gpt-4o-mini",
                           industry       = NULL,
                           brand_reach    = "global",
                           reach_country  = NULL,
                           reach_region   = NULL,
                           reach_postcode = NULL) {
  
  get_data <- all_queries(brand_name,
                          industry       = industry,
                          model          = model,
                          brand_reach    = brand_reach,
                          reach_country  = reach_country,
                          reach_region   = reach_region,
                          reach_postcode = reach_postcode)
  
  presence   <- calculate_presence_from_prompts(brand_name, get_data$presence_data)
  perception <- calculate_perception_from_prompts(brand_name, get_data)
  prestige   <- calculate_prestige_from_prompts(brand_name, get_data)
  
  airr_scores            <- list()
  airr_scores$presence   <- presence
  airr_scores$perception <- perception
  airr_scores$prestige   <- prestige
  
  return(airr_scores)
}