#' List Stations for specified parameter from daily or hourly detail, grouped by year
#'
#' @param parameter_name Name of the parameter to count stations for
#' @param data_type Either 'daily' or 'hourly'
#' @export

list_stations_with_parameter <- function(parameter_name, data_type = "daily", threshold = 0) {
  process_data <- function(data) {
    if (nrow(data) == 0) {
      warning("No data found for the given parameter")
      return(data.frame(Istasyon = character(0), Year = character(0)))
    }

    data <- data %>%
      mutate(Date = as.Date(as.numeric(Tarih), origin = "1900-01-01")) %>%  # Convert to date
      mutate(Year = format(Date, "%Y"))  # Extract year

    # Count total and available data per station per year
    data_summary <- data %>%
      group_by(Istasyon, Year) %>%
      summarise(
        total_entries = n(),
        available_entries = sum(!is.na(.data[[parameter_name]])),  # Count non-NA values
        percentage = (available_entries / total_entries) * 100  # Calculate percentage
      ) %>%
      ungroup()

    # Filter stations based on threshold
    filtered_data <- data_summary %>%
      filter(percentage >= threshold) %>%  # Apply threshold
      select(Istasyon, Year) %>%
      mutate(Value = "x")  # Mark presence

    # Convert to wide format
    wide <- filtered_data %>%
      pivot_wider(names_from = Year, values_from = Value, values_fill = "-")

    return(wide)
  }

  if (data_type == 'daily') {
    conn <- create_postgres_conn()
    query <- paste0("SELECT * FROM daily_detail WHERE \"", parameter_name, "\" IS NOT NULL")
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data)
  } else if (data_type == 'hourly') {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data)
  }

  return(result)
}

list_stations_with_parameter("PM10", "hourly")