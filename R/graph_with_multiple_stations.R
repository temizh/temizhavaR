#' Create Dygraphs for Hourly Time Series
#'
#' This function generates a Dygraph for hourly time series data, showing the variation of a specified parameter over time for a given station or multiple stations.
#'
#' @param query_result A dataframe containing data from a database query. It should include columns for "Tarih" (date/time) and the specified parameter(s) along with the station names.
#' @param station_name A character vector specifying the name of the station(s) for which the time series graph will be created.
#' @param parameters A character vector specifying the name(s) of the parameter(s) to be plotted.
#' @return A Dygraph object displaying the hourly time series graph.
#' @export


create_hourly_time_series_graph_multiple_stations <- function(data, station_names, parameters) {

  data <- data[data$Istasyon %in% station_names, c("Tarih", parameters)]


  if (!inherits(data$Tarih, "POSIXct")) {
    data$Tarih <- as.POSIXct(data$Tarih)
  }
}




