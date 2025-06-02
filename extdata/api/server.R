library(RestRserve)

# Create API instance
app <- Application$new()

# Load middleware
source("extdata/api/middleware.R")
app$append_middleware(api_key_middleware)

# Load and register endpoints
source("extdata/api/endpoints.R")
app$add_get(path = "/get_data", FUN = get_data_handler)
app$add_post(path = "/get_data_by_config", FUN = get_data_by_config_handler)
app$add_post(path = "/create_analysis", FUN = create_analysis_handler)
app$add_get(path = "/get_stations", FUN = get_stations_handler)

# Return the API app
app