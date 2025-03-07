#' Count Stations for specified parameter from daily or hourly detail, grouped by year
#'
#' @param parameter_name Name of the parameter to count stations for
#' @param data_type Either 'daily' or 'hourly'
#' @export

list_stations_with_parameter_count <- function(parameter_name, data_type = "daily", until_year = 2023) {
  process_data <- function(data) {
    if (nrow(data) == 0) {
      warning("No data found for the given parameter")
      return(data.frame(Year = integer(0), station_count = integer(0)))
    }

    data %>%
      mutate(Year = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S")) %>% 
      mutate(Year = format(Year, "%Y")) %>% # Extract year
      filter(Year <= until_year) %>%  # Filter data until specified year
      group_by(Year) %>%
      summarise(station_count = n_distinct(Istasyon_modified)) %>%
      select(Year, sort(names(.)[-1]))  # Order columns
  }

  if (data_type == "daily") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM daily_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data)
  } else if (data_type == "hourly") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data)
  }

  return(result)
}
