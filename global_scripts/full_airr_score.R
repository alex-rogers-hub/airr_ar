# ============================================
# full_airr_score.R
# ============================================

full_air_score <- function(brand_name,
                           model          = "gpt-4o-mini",
                           industry       = NULL,
                           brand_reach    = "global",
                           reach_country  = NULL,
                           reach_region   = NULL,
                           reach_postcode = NULL,
                           brand_id       = NULL,   # new — for alias lookup
                           con            = NULL) { # new — for alias lookup
  
  get_data <- all_queries(brand_name,
                          industry       = industry,
                          model          = model,
                          brand_reach    = brand_reach,
                          reach_country  = reach_country,
                          reach_region   = reach_region,
                          reach_postcode = reach_postcode)
  
  # Build search names — main brand name + any aliases
  search_names <- if (!is.null(brand_id) && !is.null(con)) {
    get_brand_search_names(brand_id, brand_name, con)
  } else {
    brand_name  # fallback — no alias lookup
  }
  
  message(sprintf("  Presence search names: %s",
                  paste(search_names, collapse = ", ")))
  
  presence   <- calculate_presence_from_prompts(search_names,
                                                get_data$presence_data)
  perception <- calculate_perception_from_prompts(brand_name, get_data)
  prestige   <- calculate_prestige_from_prompts(brand_name, get_data)
  
  airr_scores            <- list()
  airr_scores$presence   <- presence
  airr_scores$perception <- perception
  airr_scores$prestige   <- prestige
  
  return(airr_scores)
}