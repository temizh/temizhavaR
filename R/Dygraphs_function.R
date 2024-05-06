#' Create Dygraphs
#'
#' @param query_result Dataframe containing data from database query.
#' @param station_name specified station name.
#' @export

create_hourly_time_series_graph <- function(data, parameters) {

  data <- data[, c("Tarih", parameters)]


  if (!inherits(data$Tarih, "POSIXct")) {
    data$Tarih <- as.POSIXct(data$Tarih)
  }

  dygraph(data, main = "Hourly Data") %>%
    dyAxis("y", label = "Value") %>%
    dyOptions(colors = c("blue", "red", "green", "orange", "purple", "brown", "black", "gray")) %>%
    dyLegend(labelsSeparateLines = TRUE)
}
