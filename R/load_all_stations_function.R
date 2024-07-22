#' Get all data from the hourly_detail table for all stations
#'
#' @param db_conn The SQLite database connection.
#' @return A data frame containing all data from the hourly_detail table for all stations.
#' @export

get_all_station_hourly_detail_data <- function(db_conn) {

  all_query_result <- dbGetQuery(db_conn, "SELECT * FROM hourly_detail")


  dbDisconnect(db_conn)


  return(all_query_result)
}
