#' Calculate Overall Average PM2.5 for specified parameter from daily_detail for all stations in a city
#'
#' This function calculates the overall average value of a specified parameter from daily_detail data for all stations in a city.
#'
#' @param city_name The name of the city.
#' @param parameter The parameter for which the overall average is calculated.
#' @return The overall average value of the parameter for all stations in the city.
#' @export

calculate_overall_PM25_average_by_city <- function(parameter, data_type="daily", threshold = 0, until_year = 2023) {

  # parameter =  init.temizhavaR()

  # YEAR <- options()$temizhavaR.YEAR

  # conn <- create_postgres_conn()

  # city_query <- dbGetQuery(conn, paste0("SELECT DISTINCT Sehir FROM location_", YEAR))
  # cities <- city_query$Sehir

  # overall_avgs <- lapply(cities, function(city_name) {
  #   station_query <- dbGetQuery(conn, paste0("SELECT Istasyonlar FROM location_", YEAR, " WHERE Sehir='", city_name, "'"))
  #   stations <- station_query$Istasyonlar

  #   station_data <- lapply(stations, function(station) {
  #     pm25_data_percentage_query <- paste0("SELECT (SUM(CASE WHEN PM25 IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail WHERE Istasyon='", station, "'")
  #     pm25_data_percentage <- dbGetQuery(conn, pm25_data_percentage_query)$data_percentage

  #     if (!is.na(pm25_data_percentage) && pm25_data_percentage >= 75) {
  #       #Take the PM2.5 measurement
  #       pm25_query <- paste0("SELECT AVG(PM25) AS Yillik_Ortalama FROM daily_detail WHERE Istasyon='", station, "'")
  #       pm25 <- dbGetQuery(conn, pm25_query)

  #       pm25_values <- data.frame(Istasyon = station, PM25 = pm25$Yillik_Ortalama,
  #                                 pm25_veri_mevcudiyet_yuzdesi = pm25_data_percentage,
  #                                 pm10_veri_mevcudiyet_yuzdesi = NA,
  #                                 pm10_value = NA)

  #     } else {

  #       #Estimate PM2.5 from PM10 measurements if PM10 satisfies 90 availability treshold
  #       pm25_values <- new_pm25_for_city_based_mean(station, verbose = TRUE)
  #     }

  #     pm25_values
  #   })

  #   station_data <- do.call("rbind", station_data)
  #   #print(paste(city_name, ", class : ", class(station_data), ", dim : ", dim(station_data) ))
  #   station_data <- cbind(data.frame(Sehir = NA), station_data)
  #   station_data$Sehir <- city_name

  #   #if (station == "Erzurum - Palandöken") browser()
  #   #if (city_name  == "Erzurum") browser()

  #   station_data
  # })

  # disconnect_postgres(conn)

  # result <- do.call("rbind", overall_avgs) %>%
  #   #arrange(desc(PM25)) %>%
  #   mutate(PM25 = round(PM25, 5)) %>%
  #   mutate(pm10_value = round(pm10_value, 5))

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
