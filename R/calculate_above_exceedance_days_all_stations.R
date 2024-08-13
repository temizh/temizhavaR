#' Calculate Stations with Annual Average Above Threshold and Their Exceedance Days
#'
#' This function calculates the annual average concentration of a specified air quality parameter
#' for stations that have a data availability percentage above 90%. It returns
#' the stations whose annual average exceeds a specified threshold along with the number of days
#' the parameter exceeded that threshold.
#'
#' @param parameter The name of the air quality parameter (e.g., "PM10", "SO2") to be analyzed.
#' @param pollutant_threshold The concentration threshold for the annual average. Stations with an annual average
#' above this value will be returned. (e.g., 40 µg/m³)
#' @param data_threshold The minimum data availability percentage required for a station to be included in the analysis. Default is 90.
#' @return A data frame containing the stations with an annual average above the specified threshold and the number of exceedance days.
#' @export
#'
#' @examples
#' # Calculate stations with annual average PM10 above 40 µg/m³ and at least 90% data availability
#' result <- calculate_above_exceedance_days_all_stations("PM10", 40)
#' print(result)

calculate_above_exceedance_days_all_stations <- function(parameter, pollutant_threshold, data_threshold = 90) {
  # Get stations with sufficient data availability
  valid_stations <- daily_list_stations_with_parameter_threshold(parameter_name = parameter, availability_threshold = data_threshold)

  # Filter stations with data availability above the threshold
  valid_station_names <- valid_stations$Istasyon[valid_stations$threshold_status == "Üstünde"]

  # Connect to the database and load the daily data
  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
  daily_data <- dbReadTable(mydb, "daily_detail")
  dbDisconnect(mydb)

  # Clean column names and parameter name
  names(daily_data) <- gsub("^\"|\"$", "", names(daily_data))
  names(daily_data) <- trimws(names(daily_data))
  parameter <- gsub("^\"|\"$", "", parameter)
  parameter <- trimws(parameter)

  # Calculate annual averages and exceedance days
  result <- daily_data %>%
    filter(Istasyon %in% valid_station_names) %>%
    filter(!is.na(.data[[parameter]])) %>%
    group_by(Istasyon) %>%
    summarise(
      annual_average = mean(.data[[parameter]], na.rm = TRUE),
      exceedance_days = sum(.data[[parameter]] > pollutant_threshold, na.rm = TRUE)
    ) %>%
    filter(annual_average > pollutant_threshold) %>%
    arrange(desc(annual_average))

  return(result)
}
