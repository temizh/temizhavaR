#' Calculate Overall Average PM2.5 for specified parameter from daily_detail for all stations in a city
#'
#' This function calculates the overall average value of a specified parameter from daily_detail data for all stations in a city.
#'
#' @param city_name The name of the city.
#' @param parameter The parameter for which the overall average is calculated.
#' @return The overall average value of the parameter for all stations in the city.
#' @export

calculate_overall_PM25_average_by_city <- function(data_type="daily", threshold = 75, by_city = FALSE, until_year = 2023) {

  process_data <- function(data, locations, total_amount) {

    factor <- 0.6667

    # join data with locations and keep city name
    data <- data %>%
      left_join(locations %>% select(Istasyon_modified, Sehir), by = c("Istasyon_modified" = "Istasyon_modified"))

    # Convert Tarih to POSIXct and extract year
    data <- data %>% 
      mutate(Tarih = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(Year = format(Tarih, "%Y")) %>% # Extract year
      filter(Year <= until_year) # Filter data until specified year

    if(by_city) {
      data <- data %>% 
        mutate(location = Sehir)
    } else {
      data <- data %>% 
        mutate(location = Istasyon_modified)
    }

    # Count total and available data per station per year
    data_summary <- data %>%
      group_by(location, Year) %>%
        summarise(
          location_amount = ifelse(by_city, n_distinct(Istasyon_modified), 1),  # Count unique locations
          PM25_average = mean(PM25, na.rm = TRUE),  # Calculate PM25 average
          PM10_average = mean(PM10, na.rm = TRUE),  # Calculate PM10 average
          available_PM25_entries = sum(!is.na(PM25)),  # Count PM25 non-NA values
          available_PM10_entries = sum(!is.na(PM10)),  # Count PM10 non-NA values
          PM25_precentage = (available_PM25_entries / (total_amount * location_amount)) * 100,  # Calculate PM25 percentage
          PM10_precentage = (available_PM10_entries / (total_amount * location_amount)) * 100,  # Calculate PM10 percentage
          PM25_precentage = ifelse(is.na(PM25_precentage), 0, PM25_precentage),  # Set 0 if NA
          PM10_precentage = ifelse(is.na(PM10_precentage), 0, PM10_precentage)  # Set 0 if NA
        ) %>%
        ungroup()

    # if PM25 precentage is below threshold, estimate PM25 from PM10 by factor
    filtered_data <- data_summary %>%
      mutate(PM25_final = ifelse(PM25_precentage < threshold, PM10_average * factor, PM25_average)) %>% # Estimate PM25 from PM10
      mutate(PM25_final = ifelse(PM25_precentage < threshold & PM10_precentage < threshold, NA, PM25_final)) %>% # Set NA if both PM25 and PM10 are below threshold
      select(location, Year, PM25_final, PM25_precentage, PM10_precentage, PM25_average, PM10_average)  # Select relevant columns

    # Rename columns
    filtered_data <- filtered_data %>%
      rename(Lokasyon = location) %>%
      rename(PM25 = PM25_final) %>%
      rename(a = PM25_precentage) %>%
      rename(b = PM10_precentage) %>%
      rename(c = PM25_average) %>%
      rename(d = PM10_average)

    # Convert to wide format
    wide <- filtered_data %>%
      pivot_wider(names_from = Year, values_from = c(PM25, a, b, c, d), values_fill = NA)

    return(wide)
  }

  conn <- create_postgres_conn()
  location_query <- 'SELECT * FROM location'
  locations <- dbGetQuery(conn, location_query)

  if (data_type == "daily") {
    query <- 'SELECT * FROM daily_detail'
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, locations, 365)
  } else if (data_type == "hourly") {
    query <- 'SELECT * FROM hourly_detail'
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, locations, 365 * 24)
  }

  return(result)
}
