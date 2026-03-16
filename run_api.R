# run_api.R — start this as a background process on the compute server

library(plumber)

pr <- plumb("/home/aarogers/airr/api.R")

pr$run(
  host = "0.0.0.0",
  port = 8765,
  swagger = FALSE    # disable public swagger UI in production
)