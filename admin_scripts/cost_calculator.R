#' Calculate estimated daily AI cost for a user
#' @param n_competitors  Number of competitor brands tracked
#' @param n_prompts      Number of tracked prompts
#' @param n_personas     Number of customer personas
#' @param model          Model name (affects pricing)
#' @return List with cost breakdown
calculate_user_daily_cost <- function(n_competitors  = 0,
                                      n_prompts      = 0,
                                      n_personas     = 0,
                                      model          = "gpt-4.1-mini") {
  
  n_brands <- n_competitors + 1  # +1 for main brand
  
  # ── Token pricing (per token) ────────────────────────────────────────
  pricing <- switch(model,
                    "gpt-4.1-mini" = list(input = 0.40 / 1e6, output = 0.80 / 1e6),
                    "gpt-4o-mini"  = list(input = 0.15 / 1e6, output = 0.60 / 1e6),
                    "gpt-4o"       = list(input = 2.50 / 1e6, output = 10.0 / 1e6),
                    "gpt-4.1"      = list(input = 2.00 / 1e6, output = 8.00 / 1e6),
                    list(input = 0.40 / 1e6, output = 0.80 / 1e6)  # default
  )
  
  # ── Brand scoring ────────────────────────────────────────────────────
  # 220 prompts per brand, ~50 input / ~100 output tokens each
  brand_input_tokens  <- n_brands * 220 * 50
  brand_output_tokens <- n_brands * 220 * 100
  brand_cost          <- (brand_input_tokens  * pricing$input) +
    (brand_output_tokens * pricing$output)
  
  # ── Prompt scoring ───────────────────────────────────────────────────
  # 10 requests per prompt per brand, ~30 input / ~150 output tokens
  prompt_input_tokens  <- n_prompts * n_brands * 10 * 30
  prompt_output_tokens <- n_prompts * n_brands * 10 * 150
  prompt_cost          <- (prompt_input_tokens  * pricing$input) +
    (prompt_output_tokens * pricing$output)
  
  # ── Persona brand scoring ────────────────────────────────────────────
  # 80 prompts per persona per brand, ~50 input / ~100 output tokens
  persona_brand_input  <- n_personas * n_brands * 80 * 50
  persona_brand_output <- n_personas * n_brands * 80 * 100
  persona_brand_cost   <- (persona_brand_input  * pricing$input) +
    (persona_brand_output * pricing$output)
  
  # ── Persona prompt scoring ───────────────────────────────────────────
  # 10 requests per persona per prompt per brand
  persona_prompt_input  <- n_personas * n_prompts * n_brands * 10 * 30
  persona_prompt_output <- n_personas * n_prompts * n_brands * 10 * 150
  persona_prompt_cost   <- (persona_prompt_input  * pricing$input) +
    (persona_prompt_output * pricing$output)
  
  total_daily  <- brand_cost + prompt_cost + persona_brand_cost + persona_prompt_cost
  total_monthly <- total_daily * 30
  
  list(
    model          = model,
    n_brands       = n_brands,
    n_prompts      = n_prompts,
    n_personas     = n_personas,
    daily  = list(
      brand_scoring          = round(brand_cost,          4),
      prompt_scoring         = round(prompt_cost,         4),
      persona_brand_scoring  = round(persona_brand_cost,  4),
      persona_prompt_scoring = round(persona_prompt_cost, 4),
      total                  = round(total_daily,         4)
    ),
    monthly = list(
      brand_scoring          = round(brand_cost          * 30, 2),
      prompt_scoring         = round(prompt_cost         * 30, 2),
      persona_brand_scoring  = round(persona_brand_cost  * 30, 2),
      persona_prompt_scoring = round(persona_prompt_cost * 30, 2),
      total                  = round(total_monthly,           2)
    )
  )
}

#' Calculate cost across all active users
calculate_all_users_cost <- function(model = "gpt-4.1-mini") {
  
  users <- dbGetQuery(pool, "
    SELECT 
      u.login_id,
      u.email,
      u.date_added,
      MAX(b.brand_name)        AS brand_name,
      MAX(ds.subscription_name) AS subscription_name,
      COUNT(DISTINCT CASE 
        WHEN ubt2.main_brand_flag = FALSE 
          AND ubt2.date_valid_from <= CURRENT_DATE
          AND (ubt2.date_valid_to IS NULL OR ubt2.date_valid_to >= CURRENT_DATE)
        THEN ubt2.brand_id END
      ) AS n_competitors,
      COUNT(DISTINCT CASE
        WHEN uqt.date_valid_from <= CURRENT_DATE
          AND (uqt.date_valid_to IS NULL OR uqt.date_valid_to >= CURRENT_DATE)
        THEN uqt.query_id END
      ) AS n_prompts,
      COUNT(DISTINCT CASE
        WHEN upt.date_valid_from <= CURRENT_DATE
          AND (upt.date_valid_to IS NULL OR upt.date_valid_to >= CURRENT_DATE)
        THEN upt.profile_id END
      ) AS n_personas
    FROM dim_user u
    LEFT JOIN fact_user_brands_tracked ubt
      ON ubt.login_id = u.login_id
      AND ubt.main_brand_flag = TRUE
      AND ubt.date_valid_from <= CURRENT_DATE
      AND (ubt.date_valid_to IS NULL OR ubt.date_valid_to >= CURRENT_DATE)
    LEFT JOIN dim_brand b 
      ON b.brand_id = ubt.brand_id
    LEFT JOIN fact_user_sub_level fus
      ON fus.login_id = u.login_id
      AND fus.date_valid_from <= CURRENT_DATE
      AND fus.date_valid_to >= CURRENT_DATE
    LEFT JOIN dim_subscription ds
      ON ds.subscription_level_id = fus.subscription_level_id
    LEFT JOIN fact_user_brands_tracked ubt2
      ON ubt2.login_id = u.login_id
    LEFT JOIN fact_user_queries_tracked uqt
      ON uqt.login_id = u.login_id
    LEFT JOIN fact_user_profiles_tracked upt
      ON upt.login_id = u.login_id
    WHERE u.onboarding_complete = TRUE
    GROUP BY u.login_id, u.email, u.date_added
    ORDER BY u.login_id
  ")
  
  # Calculate cost per user — extract scalar directly
  users$daily_ai_cost <- mapply(
    function(comp, prompts, personas) {
      tryCatch({
        result <- calculate_user_daily_cost(
          n_competitors = as.integer(comp),
          n_prompts     = as.integer(prompts),
          n_personas    = as.integer(personas),
          model = model
        )
        as.numeric(result$daily$total)
      }, error = function(e) {
        warning(paste("Cost calc failed:", e$message))
        NA_real_
      })
    },
    users$n_competitors,
    users$n_prompts,
    users$n_personas,
    SIMPLIFY = TRUE
  )
  
  users$monthly_ai_cost <- users$daily_ai_cost * 30
  
  users
}


# Cost for a typical user
calculate_user_daily_cost(
  n_competitors = 5,
  n_prompts     = 1,
  n_personas    = 1
)

# Cost for a full enterprise user
calculate_user_daily_cost(
  n_competitors = 20,
  n_prompts     = 15,
  n_personas    = 3,
  model = "gpt-4.1-mini"
)

all_costs <- calculate_all_users_cost()
all_costs <- calculate_all_users_cost(model = "gpt-4.1-mini")


cat("Total daily AI cost:  $", round(sum(all_costs$daily_ai_cost),  2), "\n")
cat("Total monthly AI cost: $", round(sum(all_costs$monthly_ai_cost), 2), "\n")
cat("Average per user/month: $", 
    round(mean(all_costs$monthly_ai_cost), 2), "\n")



starter_cost <- calculate_user_daily_cost(
  n_competitors = 5,
  n_prompts     = 1,
  n_personas    = 1
)

pro_cost <- calculate_user_daily_cost(
  n_competitors = 10,
  n_prompts     = 10,
  n_personas    = 3
)

sfa_cost <- calculate_user_daily_cost(
  n_competitors = 5,
  n_prompts     = 3,
  n_personas    = 0
)


starter_profit <- 99 - starter_cost$monthly$total
pro_profit <- 349 - pro_cost$monthly$total

# profit perc
# starter_profit/99
# pro_profit / 349

cat("Total Starter Profit per User:  $", round(starter_profit,  2), "\n")
cat("Total Pro Profit per User:  $", round(pro_profit,  2), "\n")

10000/starter_profit
10000/pro_profit
