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
valid_key <- Sys.getenv("API_KEY")

api_key_middleware <- Middleware$new(
  process_request = function(.req, .res) {
    # Get the API key from the request headers
    api_key <- .req$get_header("X-API-Key")

    # Check if the API key exists
    if (is.null(api_key)) {
      # Respond with a 401 Unauthorized error
      raise(HTTPError$unauthorized())
    }

    # Validate API key
    if (api_key != valid_key) {
      # Respond with a 403 Forbidden error
      raise(HTTPError$forbidden())
    }
  },
  id = "api_key"
)
