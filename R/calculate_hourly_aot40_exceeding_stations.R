#' Calculate Hourly Exceeding Stations
#'
#' This function calculates the number of stations that exceed a specified threshold
#' for a given parameter over a specified hourly window and within certain months,
#' or for the entire year if no months are specified. The calculation can be done
#' on a daily or monthly basis.
#'
#' @param parameter_name A string representing the name of the parameter (pollutant) to analyze.
#' @param hour_window An integer representing the number of hours for the rolling average window. Default is 1.
#' @param months A vector of integers representing the months to include in the analysis. Default is c(4, 5, 6, 7, 8, 9) for April to September.
#' Set to NULL for the entire year.
#' @param threshold A numeric value representing the threshold value for the parameter. Default is 180.
#' @param aggregation_period A string representing the aggregation period, either "daily" or "monthly". Default is "daily".
#'
#' @return A data.frame with the number of stations that exceed the threshold for the specified parameter.
#' @export

calculate_hourly_aot40_exceeding_stations <- function(parameter_name, hour_window = 1, months = c(4, 5, 6, 7, 8, 9), threshold = 180, aggregation_period = "daily") {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT Istasyon, Tarih, ", parameter_name, " FROM hourly_detail")
  data <- dbGetQuery(mydb, query)

  data <- data %>%
    mutate(TarihSaat = as.POSIXct(Tarih, format="%Y-%m-%d %H:%M:%S"),
           month = month(TarihSaat))

  if (!is.null(months)) {
    data <- data %>%
      filter(month %in% months)
  }

  if (hour_window > 1) {
    data <- data %>%
      arrange(Istasyon, TarihSaat) %>%
      group_by(Istasyon) %>%
      mutate(rolling_avg = zoo::rollapplyr(.data[[parameter_name]], width = hour_window, FUN = mean, fill = NA, align = "right")) %>%
      ungroup()
  } else {
    data <- data %>%
      mutate(rolling_avg = .data[[parameter_name]])
  }

  if (aggregation_period == "daily") {
    aggregated_data <- data %>%
      mutate(date = as.Date(TarihSaat)) %>%
      group_by(Istasyon, date) %>%
      summarise(max_avg = if(all(is.na(rolling_avg))) NA else max(rolling_avg, na.rm = TRUE)) %>%
      ungroup()
  } else if (aggregation_period == "monthly") {
    aggregated_data <- data %>%
      mutate(year_month = format(TarihSaat, "%Y-%m")) %>%
      group_by(Istasyon, year_month) %>%
      summarise(max_avg = if(all(is.na(rolling_avg))) NA else max(rolling_avg, na.rm = TRUE)) %>%
      ungroup()
  } else {
    stop("Invalid aggregation_period. Choose either 'daily' or 'monthly'.")
  }

  exceed_stations <- aggregated_data %>%
    filter(!is.na(max_avg) & max_avg > threshold) %>%
    select(Istasyon) %>%
    distinct()

  num_exceeding_stations <- nrow(exceed_stations)

  result_df <- data.frame(num_exceeding_stations = num_exceeding_stations)

  dbDisconnect(mydb)

  return(result_df)
}
