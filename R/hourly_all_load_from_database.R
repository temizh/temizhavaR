#' Hourly detail load from database
#'
#' @param station_name The name of the station to filter the data.
#' @export




all_hourly_detail_load_from_database <- function() {
 
 
  conn <- create_postgres_conn()

  query <- "SELECT * FROM hourly_detail"
  query_result <- dbGetQuery(conn, query)


  query_result$Tarih <- as.POSIXct(query_result$Tarih, format = "%Y-%m-%d %H:%M:%S")


  disconnect_postgres(conn)


  return(query_result)
}
