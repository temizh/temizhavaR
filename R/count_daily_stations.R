#' Count Stations for specified parameter from daily_detail
#'
#' @param data A pair of numbers.
#' @export



daily_list_stations_with_parameter_count <- function(parameter_name) {

  conn <- create_postgres_conn()

  query <- paste0("SELECT DISTINCT Istasyon FROM daily_detail WHERE ", parameter_name, " IS NOT NULL")

  query_result <- dbGetQuery(conn, query)

  disconnect_postgres(conn)

  return(data.frame(nrow(query_result)))
}
