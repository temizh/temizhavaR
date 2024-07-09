#' Create Dygraphs
#'
#' @param data Dataframe containing data to be graphed
#' @param station_name specified station name
#' @param parameters parameters to be graphed
#' @export

create_hourly_time_series_graph <- function(data, station_name, parameters) {

  data <- data[, c("Tarih", parameters)]

  if (!inherits(data$Tarih, "POSIXct")) {
    data$Tarih <- as.POSIXct(data$Tarih)
  }


  dygraph(data, main = station_name) %>%
    dyAxis("y", label = parameters) %>%
    dyOptions(colors = c("blue", "red", "green", "orange", "purple", "brown", "black", "gray")) %>%
    dyLegend(labelsSeparateLines = TRUE)
}











# create_hourly_time_series_graph <- function(data, station_name, parameters) {
#
#   data <- data[, c("Tarih", parameters)]
#
#   if (!inherits(data$Tarih, "POSIXct")) {
#     data$Tarih <- as.POSIXct(data$Tarih)
#   }
#
#
#   dygraph(data, main = station_name) %>%
#     dyAxis("y", label = "Value") %>%
#     dyOptions(colors = c("blue", "red", "green", "orange", "purple", "brown", "black", "gray")) %>%
#     dyLegend(labelsSeparateLines = TRUE)
# }
