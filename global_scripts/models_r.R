# models_r.R - Data structures to accompany prestige.R

create_brand_profile <- function(name, aliases = character(0), industry = NULL) {
  list(
    name = name,
    aliases = aliases,
    industry = industry,
    all_names = c(name, aliases)
  )
}

create_competitor_set <- function(primary_brand, competitors) {
  list(
    primary_brand = primary_brand,
    competitors = competitors
  )
}

create_llm_response <- function(prompt_id, prompt_text, response_text, 
                                model = "gpt-4o-mini", timestamp = Sys.time()) {
  tibble(
    prompt_id = prompt_id,
    prompt_text = prompt_text,
    response_text = response_text,
    model = model,
    timestamp = timestamp
  )
}

# Validator for prestige results
validate_prestige_score <- function(score_obj) {
  stopifnot(
    is.list(score_obj),
    "score" %in% names(score_obj),
    score_obj$score >= 0 && score_obj$score <= 100,
    "competitive_rank" %in% names(score_obj),
    "authority_signals" %in% names(score_obj),
    "category_leadership" %in% names(score_obj)
  )
  invisible(score_obj)
}