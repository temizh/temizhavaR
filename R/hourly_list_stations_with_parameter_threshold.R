#' List Stations for specified parameter from hourly_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @export
hourly_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 8761) AS data_percentage FROM hourly_detail GROUP BY Istasyon) WHERE data_percentage >= ", threshold)

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  return(query_result)
}
