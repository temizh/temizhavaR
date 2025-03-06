#' Create Hourly Dygraphs
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

#' Create Daily Dygraphs
#'
#' @param data Dataframe containing data to be graphed
#' @param station_name specified station name
#' @param parameters parameters to be graphed
#' @export

create_daily_time_series_graph <- function(data, file_path, parameters, is_panel = FALSE) {

  # Convert from wide to long format
  df_long <- data %>%
    select(Tarih, all_of(parameters)) %>%  # Keep only 'date' and series columns
    pivot_longer(cols = -Tarih, names_to = "Series", values_to = "Value")

  # if parameter is one value
  if (length(parameters) == 1) {
    graph_name <- paste("Time Series Plot for", parameters)
  } else {
    graph_name <- paste("Multi-Parameter Time Series Plot for", paste(parameters, collapse = ", "))
  }

  # Create multi-line time series plot
  p <- ggplot(df_long, aes(x = Tarih, y = Value, color = Series)) +
    geom_line(size = 0.2) +
    geom_point(size = 0.4) +
    labs(title = graph_name, x = "Date", y = "Value", color = "Series") +
    theme_light()

  if (is_panel) {
    p <- p + facet_wrap(~ Series, scales = "free_y", ncol = 1)
  }

  # Save the plot as a PNG file
  ggsave(file_path, plot = p, width = 16, height = 6, dpi = 300)
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
