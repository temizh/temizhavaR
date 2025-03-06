#' Calculate Rolling 8-Hour Averages for Ozone (O3)
#'
#' This function retrieves O3 data from a given SQLite database,
#' and calculates rolling 8-hour averages for stations with 90% or higher
#' data availability.
#' @export


calculate_rolling_8hour_avg <- function() {
  # Connect to the database
  conn <- create_postgres_conn()

  # Query hourly_detail table for the relevant data
  hourly_data <- dbGetQuery(conn,
                            "SELECT Istasyon, Tarih, O3
                             FROM hourly_detail
                             WHERE Tarih BETWEEN '2023-01-01 00:00:56' AND '2024-01-01 00:00:56'
                             ORDER BY Istasyon, Tarih")

  # Calculate data availability for each station
  station_data_availability <- hourly_data %>%
    group_by(Istasyon) %>%
    summarise(data_count = sum(!is.na(O3)),
              total_hours = n()) %>%
    mutate(availability = (data_count / total_hours) * 100) %>%
    filter(availability >= 90)

  # Filter data to include only stations with 90% or higher data availability
  filtered_data <- hourly_data %>%
    filter(Istasyon %in% station_data_availability$Istasyon)

  # Calculate 8-hour rolling averages for O3
  result <- filtered_data %>%
    group_by(Istasyon) %>%
    arrange(Tarih) %>%
    mutate(O3_8hour_avg = rollmean(O3, k = 8, align = "right", fill = NA)) %>%
    ungroup() %>%
    select(Istasyon, Tarih, O3_8hour_avg)

  # Disconnect from the database
  disconnect_postgres(conn)

  return(result)
}

