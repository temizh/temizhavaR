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

    # Check if the API key exists
    if (is.null(api_key)) {
      # Respond with a 401 Unauthorized error
      .res$set_status_code(401)
      .res$set_body("Missing API key.")
      return(FALSE)  # Stop the request processing
    }

    # Validate API key
    if (api_key != valid_key) {
      # Respond with a 403 Forbidden error
      .res$set_status_code(403)
      .res$set_body("Invalid API key.")
      return(FALSE)  # Stop the request processing
    }

    return(TRUE)  # Continue processing the request
  },
  id = "api_key"
)
