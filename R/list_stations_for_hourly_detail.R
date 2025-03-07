#' List Stations for specified parameter from hourly_detail
#'
#' @param data A pair of numbers.
#' @export

hourly_list_stations_with_parameter <- function(parameter_name) {

  conn <- create_postgres_conn()
  if (is.null(conn)) {
    stop("Could not establish database connection")
  }

  query <- paste0("SELECT DISTINCT Istasyon_modified FROM hourly_detail WHERE ", parameter_name, " IS NOT NULL")

  query_result <- dbGetQuery(conn, query)

  disconnect_postgres(conn)

  return(query_result)
}
