#' Station Average
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability is calculated (default is NULL). Use "summer" or "winter".
#' @return A data frame with station names, their data availability percentages, and parameter averages, sorted by data availability.
#' @export

station_average <- function(parameter_name, data_type="daily", threshold = 90, season = NULL, until_year = 2023) {

  process_data <- function(data, data_type) {

    data <- data %>% 
    mutate(Tarih = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(Year = format(Tarih, "%Y")) %>% # Extract year
    filter(Year <= until_year) %>%  # Filter data until specified year
    mutate(month = as.numeric(format(Tarih, "%m"))) # Extract month

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
      group_by(Istasyon_modified, Year) %>%
      summarise(
        available_entries = sum(!is.na(.data[[parameter_name]])),  # Count non-NA values
        non_na_days = sum(!is.na(.data[[parameter_name]])), # Count non-NA values
        percentage = round(non_na_days / data_count_in_season * 100, 2),  # Calculate percentage
        average = mean(.data[[parameter_name]], na.rm = TRUE)
      ) %>%
      ungroup()

    # Filter stations based on threshold
    filtered_data <- data_summary %>%
      filter(percentage >= threshold) %>%  # Apply threshold
      mutate(Value = average) %>%  # Mark presence
      select(Istasyon_modified, Year, Value)  # Select relevant columns

    # Convert to wide format
    wide <- filtered_data %>%
      pivot_wider(names_from = Year, values_from = Value, values_fill = NA) %>%
      select(Istasyon_modified, sort(names(.)[-1]))  # Order columns

    return(wide)
  }

  if (data_type == "daily") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM daily_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, data_type)
  } else if (data_type == "hourly") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, data_type)
  }

  return(result)
}
