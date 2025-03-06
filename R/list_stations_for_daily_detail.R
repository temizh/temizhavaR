#' List Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter to check
#' @return A dataframe of distinct station names
#' @export
daily_list_stations_with_parameter <- function(parameter_name) {
  tryCatch({
    conn <- create_postgres_conn()
    if (is.null(conn)) {
      stop("Could not establish database connection")
    }

    stations <- tbl(conn, "daily_detail") %>%
      filter(!is.na(.data[[parameter_name]])) %>%
      distinct(Istasyon) %>%
      collect()

    disconnect_postgres(conn)

    return(stations)
  }, error = function(e) {
    message("Error in daily_list_stations_with_parameter: ", e$message)
    return(NULL)
  })
}
