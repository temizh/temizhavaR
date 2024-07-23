#' Count Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The specific season to consider (e.g., "summer" for nisan, mayıs, haziran, temmuz, ağustos, eylül).
#' @export

daily_count_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")


  if (!is.null(season) && season == "summer") {

    season_filter <- "AND strftime('%m', Tarih) IN ('04', '05', '06', '07', '08', '09')"
    days_in_season <- 183

  } else if (!is.null(season) && season == "winter") {

    season_filter <- "AND strftime('%m', Tarih) IN ('01', '02', '03', '10', '11', '12')"
    days_in_season <- 182
  }

    else {

    season_filter <- ""
    days_in_season <- 365
  }

  query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / ", days_in_season, ") AS data_percentage FROM daily_detail WHERE 1=1 ", season_filter, " GROUP BY Istasyon) WHERE data_percentage >= ", threshold)

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  return(data.frame(nrow(query_result)))
}
