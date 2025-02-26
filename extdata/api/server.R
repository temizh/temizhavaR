library(RestRserve)

# Create API instance
app <- Application$new()

# Load middleware
source("middleware.R")
app$append_middleware(api_key_middleware)

# Load and register endpoints
source("endpoints.R")
app$add_get(path = "/get_data", FUN = get_data_handler)
app$add_post(path = "/get_data_by_config", FUN = get_data_by_config_handler)

# Return the API app
app