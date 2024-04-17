#' Count Stations
#'
#' @param processed_data processed data.
#' @param param_name specified paramater name.
#' @export

count_stations <- function(processed_data, param_name) {
  data <- processed_data$data
  station_name <- processed_data$station_name
  param_names <- processed_data$param_names


if (!is.null(station_name)) {
  if (param_name %in% param_names) {
    if (param_name %in% colnames(data)) {
      if (any(!is.na(data[[param_name]]))) {
        cat(paste0(param_name, " stations:\n"))
        cat(station_name, "\n")
        num_stations <- length(station_name)
        cat(paste0("Total number of stations: ", num_stations, "\n"))
      } else {
        cat("No station found containing any value for the specified parameter.\n")
      }
    } else {
      cat(paste0(param_name, " parameter not found.\n"))
    }
  } else {
    cat(paste0("Invalid parameter: ", param_name, ".\n"))
  }
} else {
  cat("Station names not found.\n")
}
  num_stations
}
