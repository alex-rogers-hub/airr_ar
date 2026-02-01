install.packages("reticulate")
library(reticulate)

# Verify Python configuration
py_config()

# Test it works
py_run_string("print('Hello from Python!')")