

full_air_score <- function(brand_name, model = "gpt-4o-mini"){
  
  get_data <- all_queries(brand_name, model = model)
  
  presence <- calculate_presence_from_prompts(brand_name,
                                              get_data$presence_data)
  
  perception <- calculate_perception_from_prompts(brand_name,
                                                  get_data)
  
  prestige <- calculate_prestige_from_prompts(brand_name,
                                              get_data)
  
  airr_scores <- list()
  
  airr_scores$presence <- presence
  airr_scores$perception <- perception
  airr_scores$prestige <- prestige
  
  return(airr_scores)
}





