#' Calculate Exceedance Days for specified parameter from hourly_detail for all stations
#'
#' This function calculates the number of days exceeding a specified threshold for a given parameter from daily_detail data for all stations.
#'
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter.
#' @param exceedance_limit The minimum number of hours a station must exceed the threshold to be considered.
#' @return A data frame containing the station names and the number of days exceeding the specified threshold for the parameter.
#' @export

hourly_above_exceedance_days_double_threshold <- function(parameter, threshold, exceedance_limit) {
  conn <- create_postgres_conn()
  stations_query <- paste0("SELECT Istasyon
                            FROM hourly_detail
                            GROUP BY Istasyon
                            HAVING (SUM(CASE WHEN ", parameter, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 8761) >= 90")

  stations <- dbGetQuery(conn, stations_query)

  query <- paste0("SELECT Istasyon, SUM(CASE WHEN ", parameter, " > ", threshold, " THEN 1 ELSE 0 END) AS ExceedanceDays
                   FROM hourly_detail
                   WHERE Istasyon IN ('", paste(stations$Istasyon, collapse = "','"), "')
                   GROUP BY Istasyon")

  query_result <- dbGetQuery(conn, query)
  disconnect_postgres(conn)
  exceedance_days_filtered <- query_result[query_result$ExceedanceDays > exceedance_limit, ]
  return(exceedance_days_filtered)
}
