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
app$add_get(path = "/get_intermediate_analysis", FUN = get_intermediate_analysis_handler)
app$add_get(path = "/get_final_analysis", FUN = get_final_analysis_handler)
app$add_post(path = "/create_intermediate_analysis", FUN = create_intermediate_analysis_handler)
app$add_post(path = "/delete_intermediate_analysis", FUN = delete_intermediate_analysis_handler)
app$add_post(path = "/create_final_analysis", FUN = create_final_analysis_handler)
app$add_post(path = "/delete_final_analysis", FUN = delete_final_analysis_handler)
app$add_get(path = "/get_analysis_configs", FUN = get_analysis_configs_handler)

# Return the API app
app