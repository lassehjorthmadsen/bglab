# run_api.R

# Load required packages
library(plumber)
library(backgammon)

# Read the API definition
api <- plumber::plumb("api/plumber.R")

# Start the API server
api$run(port = 8000)
