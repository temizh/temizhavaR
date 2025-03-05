#' Calculate Overall Average for specified parameter from daily_detail for all stations in a city
#'
#' This function calculates the overall average value of a specified parameter from daily_detail data for all stations in a city.
#'
#' @param city_name The name of the city.
#' @param parameter The parameter for which the overall average is calculated.
#' @return The overall average value of the parameter for all stations in the city.
#' @export


calculate_overall_average_by_city_threshold <- function(parameter, data_type="daily", threshold = 0, until_year = 2023) {

  # init.temizhavaR()
  # YEAR <- options()$temizhavaR.YEAR

  # conn <- create_postgres_conn()

  # city_query <- dbGetQuery(conn, paste0("SELECT DISTINCT Sehir FROM location_", YEAR))
  # cities <- city_query$Sehir

  # overall_avgs <- sapply(cities, function(city_name) {
  #   station_query <- dbGetQuery(conn, paste0("SELECT Istasyonlar FROM location_", YEAR, " WHERE Sehir='", city_name, "'"))
  #   stations <- unlist(strsplit(station_query$Istasyonlar, ","))

  #   station_avgs <- sapply(stations, function(station) {
  #     data_percentage_query <- paste0("SELECT (SUM(CASE WHEN \"", parameter, "\" IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail WHERE Istasyon='", station, "'")
  #     data_percentage <- dbGetQuery(conn, data_percentage_query)$data_percentage

  #     if (!is.na(data_percentage) && data_percentage >= 90) {
  #       parameter_query <- paste0("SELECT \"", parameter, "\" FROM daily_detail WHERE Istasyon='", station, "'")
  #       parameter_values <- dbGetQuery(conn, parameter_query)[[parameter]]
  #       return(parameter_values)
  #     } else {
  #       return(NA)
  #     }
  #   }, USE.NAMES = FALSE)

  #   station_avgs <- unlist(station_avgs)
  #   station_avgs <- station_avgs[!is.na(station_avgs)]

  #   if (length(station_avgs) > 0) {
  #     overall_avg <- mean(station_avgs, na.rm = TRUE)
  #   } else {
  #     overall_avg <- NA
  #   }

  #   return(overall_avg)
  # })

  # disconnect_postgres(conn)

  # result <- data.frame(Sehir = cities, Ortalama = overall_avgs)

  # row.names(result) <- NULL

  # result %>%
  #   arrange(desc(Ortalama)) %>% mutate(Ortalama = round(Ortalama, 2))

  process_data <- function(data, locations, total_amount) {

    # join data with locations and keep city name
    data <- data %>%
      left_join(locations %>% select(Istasyonlar, Sehir), by = c("Istasyon" = "Istasyonlar"))

    data <- data %>% 
    mutate(Tarih = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(Year = format(Tarih, "%Y")) %>% # Extract year
    filter(Year <= until_year) %>%  # Filter data until specified year
    mutate(month = as.numeric(format(Tarih, "%m"))) # Extract month

    # Count total and available data per station per year
    data_summary <- data %>%
      group_by(Sehir, Year) %>%
        summarise(
          available_entries = sum(!is.na(.data[[parameter]])),  # Count non-NA values
          non_na_days = sum(!is.na(.data[[parameter]])), # Count non-NA values
          percentage = round(non_na_days / total_amount * 100, 2),  # Calculate percentage
          average = mean(.data[[parameter]], na.rm = TRUE)
        ) %>%
        ungroup()

      # Filter stations based on threshold
      filtered_data <- data_summary %>%
        filter(percentage >= threshold) %>%  # Apply threshold
        mutate(Value = paste0(as.character(average))) %>%  # Mark presence
        select(Sehir, Year, Value)  # Select relevant columns

      # Convert to wide format
      wide <- filtered_data %>%
        pivot_wider(names_from = Year, values_from = Value, values_fill = "-") %>%
        select(Sehir, sort(names(.)[-1]))  # Order columns

      return(wide)
  }

  conn <- create_postgres_conn()
  location_query <- 'SELECT * FROM location'
  locations <- dbGetQuery(conn, location_query)

  if (data_type == "daily") {
    query <- sprintf('SELECT * FROM daily_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, locations, 365)
  } else if (data_type == "hourly") {
    query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, locations, 365 * 24)
  }

  return(result)
}
