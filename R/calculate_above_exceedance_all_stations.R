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

calculate_above_exceedance_all_stations <- function(parameter, data_type, pollutant_threshold, data_threshold = 90, exceedance_count = 1, until_year = 2023) {
  # Get stations with sufficient data availability
  # valid_stations <- daily_list_stations_with_parameter_threshold(parameter_name = parameter, availability_threshold = data_threshold)

  # # Filter stations with data availability above the threshold
  # valid_station_names <- valid_stations$Istasyon[valid_stations$threshold_status == "Üstünde"]

  # # Connect to the database and load the daily data
  # conn <- create_postgres_conn()
  # daily_data <- dbGetQuery(conn, "SELECT * FROM daily_detail")
  # disconnect_postgres(conn)

  # # Clean column names and parameter name
  # names(daily_data) <- gsub("^\"|\"$", "", names(daily_data))
  # names(daily_data) <- trimws(names(daily_data))
  # parameter <- gsub("^\"|\"$", "", parameter)
  # parameter <- trimws(parameter)

  # # Calculate annual averages and exceedance days
  # result <- daily_data %>%
  #   filter(Istasyon %in% valid_station_names) %>%
  #   filter(!is.na(.data[[parameter]])) %>%
  #   group_by(Istasyon) %>%
  #   summarise(
  #     annual_average = mean(.data[[parameter]], na.rm = TRUE),
  #     exceedance_days = sum(.data[[parameter]] > pollutant_threshold, na.rm = TRUE)
  #   ) %>%
  #   filter(annual_average > pollutant_threshold) %>%
  #   arrange(desc(annual_average))

  process_data <- function(data, total_amount) {
    if (nrow(data) == 0) {
      warning("No data found for the given parameter")
      return(data.frame(Istasyon = character(0), Year = character(0)))
    }

    data <- data %>%
      mutate(Date = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%  # Convert to date
      mutate(Year = format(Date, "%Y")) %>% # Extract year
      filter(Year <= until_year)  # Filter data until specified year

    # Count total and available data per station per year above threshold
    data_summary <- data %>%
      group_by(Istasyon, Year) %>%
      summarise(
        available_entries = sum(!is.na(.data[[parameter_name]])),  # Count non-NA values
        non_na_days = sum(!is.na(.data[[parameter_name]])), # Count non-NA values
        percentage = round(non_na_days / total_amount * 100, 2),  # Calculate percentage
        exceedance_days = sum(.data[[parameter]] > pollutant_threshold, na.rm = TRUE), # Calculate exceedance days
        average = mean(.data[[parameter_name]], na.rm = TRUE)
      ) %>%
      ungroup()

    # Filter stations based on threshold
    filtered_data <- data_summary %>%
      filter(percentage >= data_threshold) %>%  # Apply threshold
      filter(average > pollutant_threshold) %>%  # Filter by pollutant threshold
      filter(exceedance_days >= exceedance_count) %>%  # Filter by exceedance days
      mutate(Value = paste0(as.character(exceedance_days))) %>%  # Mark presence
      select(Istasyon, Year, Value)  # Select relevant columns

    # Convert to wide format
    wide <- filtered_data %>%
      pivot_wider(names_from = Year, values_from = Value, values_fill = "-") %>%
      select(Istasyon, sort(names(.)[-1]))  # Order columns

    return(wide)
  }


  if (data_type == "daily") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM daily_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, 365)
  } else if (data_type == "hourly") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, 365 * 24)
  }

  return(result)
}
