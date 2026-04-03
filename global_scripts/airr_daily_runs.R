setwd("/home/aarogers/airr")
source("global.R")

library(parallel)

message("=== Daily run started: ", format(Sys.time()), " ===")
message("Running refresh loop and prompt loop in parallel...")

results <- mclapply(
  list(
    list(name = "refresh", fn = function() {
      message("=== daily_refresh_loop START: ", format(Sys.time()), " ===")
      tryCatch(
        daily_refresh_loop(model = "gpt-4.1-mini"),
        error = function(e) message("daily_refresh_loop ERROR: ", e$message)
      )
      message("=== daily_refresh_loop END: ", format(Sys.time()), " ===")
    }),
    list(name = "prompts", fn = function() {
      # Small delay so refresh loop gets DB connection first
      Sys.sleep(15)
      message("=== daily_prompt_loop START: ", format(Sys.time()), " ===")
      tryCatch(
        daily_prompt_loop(model = "gpt-4.1-mini"),
        error = function(e) message("daily_prompt_loop ERROR: ", e$message)
      )
      message("=== daily_prompt_loop END: ", format(Sys.time()), " ===")
    })
  ),
  function(item) item$fn(),
  mc.cores = 2
)

message("=== Daily run complete: ", format(Sys.time()), " ===")