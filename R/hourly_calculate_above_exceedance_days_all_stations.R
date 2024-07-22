#' Calculate Exceedance Days for specified parameter from hourly_detail for all stations
#'
#' This function calculates the number of days exceeding a specified threshold for a given parameter from daily_detail data for all stations.
#'
#' @param daily_data The daily detail data containing parameter values for each day.
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter.
#' @return A data frame containing the number of days exceeding the specified threshold for the parameter for each station.
#' @export

hourly_calculate_above_exceedance_days_all_stations <- function(parameter_name, threshold = 350, exceed_limit = 24) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT Istasyon,
                          COUNT(*) AS exceed_days,
                          SUM(CASE WHEN hourly_count > ", exceed_limit, " THEN 1 ELSE 0 END) AS exceed_count
                   FROM (
                         SELECT Istasyon,
                                DATE(tarih) AS tarih,
                                COUNT(*) AS hourly_count
                         FROM hourly_detail
                         WHERE ", parameter_name, " > ", threshold, "
                         GROUP BY Istasyon, DATE(tarih)
                        ) AS daily_counts
                   GROUP BY Istasyon")

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  return(query_result)
}
