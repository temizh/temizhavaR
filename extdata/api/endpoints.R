library(RestRserve)

source("logic.R")  # Load logic functions

get_data_handler <- function(.req, .res) {

  if (!is.null(.req$parameters_query[["frequency"]])) {
    frequency <- .req$parameters_query[["frequency"]]
  } else {
    frequency <- "daily"
  }

  if (!is.null(.req$parameters_query[["parameters"]])) {
    parameters <- .req$parameters_query[["parameters"]]
  } else {
    parameters <- c("PM10", "PM2.5", "NO2", "SO2", "CO", "O3")
  }

  if (!is.null(.req$parameters_query[["start_date"]])) {
    start_date <- .req$parameters_query[["start_date"]]
  } else {
    start_date <- "2013-01-01"
  }

  if (!is.null(.req$parameters_query[["end_date"]])) {
    end_date <- .req$parameters_query[["end_date"]]
  } else {
    end_date <- "2024-1-1"
  }

  region <- .req$parameters_query[["region"]]
  station <- .req$parameters_query[["station"]]
  station_type <- .req$parameters_query[["station_type"]]

  data <- get_data(frequency, parameters, start_date, end_date, region, station, station_type) # nolint

  .res$set_body(jsonlite::toJSON(data, auto_unbox = TRUE))
  .res$set_content_type("application/json")
}