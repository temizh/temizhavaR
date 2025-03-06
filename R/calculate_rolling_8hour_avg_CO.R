#' Calculate 8-Hour Rolling Averages for CO
#'
#' This function calculates the 8-hour rolling averages for CO levels in hourly air quality data.
#' The function only considers stations where data availability is 90% or higher.
#'
#' @return A `data.frame` containing the station name (`Istasyon`), timestamp (`Tarih`), and
#' the calculated 8-hour rolling average for CO (`CO_8hour_avg`).
#' @export



calculate_rolling_8hour_avg_CO <- function() {
  # Connect to database
  conn <- create_postgres_conn()

  # Retrieve data from the hourly_detail table within the specified date range
  hourly_data <- dbGetQuery(conn,
                            "SELECT Istasyon, Tarih, CO
                             FROM hourly_detail
                             WHERE Tarih BETWEEN '2023-01-01 00:00:56' AND '2024-01-01 00:00:56'
                             ORDER BY Istasyon, Tarih")

  # Calculate data availability for each station
  station_data_availability <- hourly_data %>%
    group_by(Istasyon) %>%
    summarise(data_count = sum(!is.na(CO)),
              total_hours = n()) %>%
    mutate(availability = (data_count / total_hours) * 100) %>%
    filter(availability >= 90)

  # Filter data to include only stations with 90% or higher data availability
  filtered_data <- hourly_data %>%
    filter(Istasyon %in% station_data_availability$Istasyon)

  # Calculate 8-hour rolling averages for CO
  result <- filtered_data %>%
    group_by(Istasyon) %>%
    arrange(Tarih) %>%
    mutate(CO_8hour_avg = rollmean(CO, k = 8, align = "right", fill = NA)) %>%
    ungroup() %>%
    select(Istasyon, Tarih, CO_8hour_avg)

  # Disconnect from the database
  disconnect_postgres(conn)

  return(result)
}



