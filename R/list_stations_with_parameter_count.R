#' Count Stations for specified parameter from daily or hourly detail, grouped by year
#'
#' @param parameter_name Name of the parameter to count stations for
#' @param data_type Either 'daily' or 'hourly'
#' @export

list_stations_with_parameter_count <- function(parameter_name, data_type = "daily") {
  process_data <- function(data) {
    if (nrow(data) == 0) {
      warning("No data found for the given parameter")
      return(data.frame(year = integer(0), station_count = integer(0)))
    }
    
    data %>%
      mutate(
        date = as.Date(as.numeric(Tarih), origin = "1900-01-01"),
        year = format(date, "%Y")
      ) %>%
      group_by(year) %>%
      summarise(station_count = n_distinct(Istasyon)) %>%
      arrange(year) %>%
      filter(year >= 2014 & year <= 2023)
  }

  if (data_type == "daily") {
    conn <- create_postgres_conn()
    query <- paste0("SELECT * FROM daily_detail WHERE \"", parameter_name, "\" IS NOT NULL")
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
