library(RestRserve)
library(dotenv)

# Set working directory to the directory of the current script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load environment variables from the .env file
dotenv::load_dot_env()

# Read the environment variables
valid_key <- Sys.getenv("API_KEY")

api_key_middleware <- Middleware$new(
  process_request = function(.req, .res) {
    # Get the API key from the request headers
    api_key <- .req$get_header("X-API-Key")

    print(paste0("API key: ", api_key, " == ", "Valid key: ", valid_key))

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
