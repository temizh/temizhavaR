library(RestRserve)

# Create API instance
app <- Application$new()

# Load middleware
source("middleware.R")
app$append_middleware(api_key_middleware)

# Load and register endpoints
source("endpoints.R")
app$add_get(path = "/get_data", FUN = get_data_handler)

# Return the API app
app