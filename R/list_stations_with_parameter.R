#' List Stations for specified parameter from daily or hourly detail, grouped by year
#'
#' @param parameter_name Name of the parameter to count stations for
#' @param data_type Either 'daily' or 'hourly'
#' @param threshold Minimum percentage of available data required (default: 0)
#' @import dplyr
#' @import tidyr
#' @export

list_stations_with_parameter <- function(parameter_name, data_type = "daily", threshold = 0, data_threshold = 0, season = NULL, until_year = 2023) {
  process_data <- function(data, data_type) {
    if (nrow(data) == 0) {
      warning("No data found for the given parameter")
      return(data.frame(Istasyon = character(0), Year = character(0)))
    }

    data <- data %>%
      mutate(Date = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%  # Convert to date
      mutate(Year = format(Date, "%Y")) %>% # Extract year
      filter(Year <= until_year) %>%  # Filter data until specified year
      mutate(month = as.numeric(format(Date, "%m")))  # Extract month

    if (!is.null(season)) {
      if (season == "summer") {
        summer_months <- c(4, 5, 6, 7, 8, 9)
        data <- data %>% filter(month %in% summer_months)
        days_in_season <- 183
      } else if (season == "winter") {
        winter_months <- c(1, 2, 3, 10, 11, 12)
        data <- data %>% filter(month %in% winter_months)
        days_in_season <- 182
      }
    } else {
      days_in_season <- 365
    }

    if (data_type == "daily") {
      data_count_in_season <- days_in_season
    } else if(data_type == "hourly") {
      data_count_in_season <- days_in_season * 24
    }

    # Count total and available data per station per year
    data_summary <- data %>%
      group_by(Istasyon, Year) %>%
      summarise(
        total_entries = data_count_in_season,  # Total entries
        available_entries = sum(!is.na(.data[[parameter_name]])),  # Count non-NA values
        percentage = (available_entries / total_entries) * 100,  # Calculate percentage
        average = mean(.data[[parameter_name]], na.rm = TRUE)
      ) %>%
      ungroup()

    # Filter stations based on threshold
    filtered_data <- data_summary %>%
      filter(percentage >= threshold) %>%  # Apply threshold
      filter(average >= data_threshold) %>%  # Filter by data threshold
      mutate(Value = floor(percentage)) %>%  # Mark presence
      select(Istasyon, Year, Value)  # Select relevant columns

    # Convert to wide format
    wide <- filtered_data %>%
      pivot_wider(names_from = Year, values_from = Value, values_fill = NA) %>%
      select(Istasyon, sort(names(.)[-1]))  # Order columns

    return(wide)
  }

  if (data_type == 'daily') {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM daily_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, data_type)
  } else if (data_type == 'hourly') {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, data_type)
  }

  return(result)
}