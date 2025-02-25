library(RestRserve)

source("logic.R")  # Load logic functions

get_data_handler <- function(.req, .res) {
  frequency <- .req$parameters_query[["frequency"]]
  parameters <- .req$parameters_query[["parameters"]]
  start_date <- .req$parameters_query[["start_date"]]
  end_date <- .req$parameters_query[["end_date"]]
  region <- .req$parameters_query[["region"]]
  station <- .req$parameters_query[["station"]]
  station_type <- .req$parameters_query[["station_type"]]

  data <- get_data(frequency, parameters, start_date, end_date, region, station, station_type) # nolint

  .res$set_body(jsonlite::toJSON(data, auto_unbox = TRUE))
  .res$set_content_type("application/json")
}