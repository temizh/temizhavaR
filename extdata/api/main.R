library(RestRserve)
library(dotenv)

# Set working directory to the directory of the current script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load environment variables from the .env file
dotenv::load_dot_env()

# Read the environment variables
api_port <- Sys.getenv("API_PORT")

# Load the API
source("server.R")

# Start the server
backend <- BackendRserve$new()
backend$start(app, http_port = as.numeric(api_port))