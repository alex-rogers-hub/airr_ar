
library(plumber)
# Load env vars
readRenviron("/home/aarogers/.Renviron")
pr <- plumb("/srv/shiny-server/AiRR/plumber.R")
pr$run(host = "0.0.0.0", port = 8766, docs = FALSE)
