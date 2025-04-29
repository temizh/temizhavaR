library(RestRserve)
library(dotenv)

# Set working directory to the directory of the current script
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base_dir <- getOption("temizhavaR.base_dir")

env_file <- file.path(base_dir, ".env")
if (file.exists(env_file)) {
  dotenv::load_dot_env(file = env_file)
  cat(".env file loaded successfully.\n")
} else {
  stop(".env file not found at: ", env_file)
}

# Read the environment variables
api_port <- Sys.getenv("API_PORT")

# Load the API
source("extdata/api/server.R")

# Start the server
backend <- BackendRserve$new()
backend$start(app, http_port = as.numeric(api_port), http_host = "0.0.0.0")