library(RestRserve)

# Load the API
source("server.R")

# Start the server
backend <- BackendRserve$new()
backend$start(app)