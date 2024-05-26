#' Calculate all stations mean of specified parameter
#'
#' @param station_name The name of the station to filter the data.
#' @export


calculate_all_stations_means <- function(data, parameter, threshold, total_days, verbose = FALSE) {
  results <- data %>%
    group_by(Istasyon) %>%
    summarise(
      mean_value = calculate_parameter_mean(cur_data(), parameter, threshold, total_days, verbose)$mean_value,
      percentage = calculate_parameter_mean(cur_data(), parameter, threshold, total_days, verbose)$percentage
    )
  return(results)
}
