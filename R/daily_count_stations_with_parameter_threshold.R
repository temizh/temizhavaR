#' Count Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which the data availability is calculated (default is "all").
#' @export
daily_count_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = "all") {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  if (season == "summer") {
    query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL AND strftime('%m', Tarih) IN ('04', '05', '06', '07', '08', '09') THEN 1 ELSE 0 END) * 100 / 183) AS data_percentage FROM daily_detail GROUP BY Istasyon) WHERE data_percentage >= ", threshold)
  } else if (season == "winter") {
    query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL AND strftime('%m', Tarih) IN ('01', '02', '03', '10', '11', '12') THEN 1 ELSE 0 END) * 100 / 182) AS data_percentage FROM daily_detail GROUP BY Istasyon) WHERE data_percentage >= ", threshold)
  } else {
    query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail GROUP BY Istasyon) WHERE data_percentage >= ", threshold)
  }

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  return(data.frame(nrow(query_result)))
}
