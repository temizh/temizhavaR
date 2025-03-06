#' Hourly detail load from database by station name
#'
#' @param station_name The name of the station to filter the data.
#' @export

daily_detail_load_from_database <- function(station_name) {
  init.temizhavaR()
  
  conn <- create_postgres_conn()
  
  if (!is.null(conn)) {
    tryCatch({
      query <- paste0("SELECT * FROM daily_detail WHERE station = $1")
      query_result <- dbGetQuery(conn, query, params = list(station_name))
      
      query_result$date <- as.POSIXct(query_result$date, format = "%Y-%m-%d")
      
      return(query_result)
    }, error = function(e) {
      message("Error executing query: ", e$message)
      return(NULL)
    }, finally = {
      disconnect_postgres(conn)
    })
  } else {
    message("Failed to establish database connection")
    return(NULL)
  }
}
