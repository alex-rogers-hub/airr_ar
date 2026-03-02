source("global.R")

# running the below runs by default in "gpt-4o-mini". Add "gpt-4o" or other model name to change this
# daily_refresh_loop(model = "gpt-4.1-mini")
# daily_prompt_loop(model = "gpt-4.1-mini")
# fallback_models = c("gpt-4o-mini", "gpt-4.1-mini", "gpt-4o")
daily_refresh_loop(model = "gpt-4.1-mini")
daily_prompt_loop(model = "gpt-4.1-mini")

