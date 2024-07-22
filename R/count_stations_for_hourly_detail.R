#' Count Stations for specified parameter from hourly_detail
#'
#' @param data A pair of numbers.
#' @export



hourly_list_stations_with_parameter_count <- function(parameter_name) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT DISTINCT Istasyon FROM hourly_detail WHERE ", parameter_name, " IS NOT NULL")

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  return(data.frame(nrow(query_result)))
}
