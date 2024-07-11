#' Count Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which the data availability is calculated (default is "all").
#' @export


daily_count_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = "all") {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  summer_months <- c(04, 05, 06, 07, 08, 09)

  query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / ", ifelse(season == "summer", "184", "365"), ") AS data_percentage FROM daily_detail")

  if (season == "summer") {
    query <- paste0(query, " WHERE strftime('%m', Tarih) IN ('04', '05', '06', '07', '08', '09')")
  }

  query <- paste0(query, " GROUP BY Istasyon) WHERE data_percentage >= ", threshold)

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  return(data.frame(station_count = nrow(query_result)))
}
