#' Calculate Exceedance Days for specified parameter from daily_detail for all stations
#'
#' This function calculates the number of days exceeding a specified threshold for a given parameter from daily_detail data for all stations.
#'
#' @param daily_data The daily detail data containing parameter values for each day.
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter.
#' @return A data frame containing the number of days exceeding the specified threshold for the parameter for each station.
#' @export

calculate_below_exceedance_days_all_stations <- function(parameter, threshold) {

  conn <- create_postgres_conn()

  stations_query <- paste0("SELECT Istasyon_modified
                            FROM daily_detail
                            GROUP BY Istasyon_modified
                            HAVING (SUM(CASE WHEN ", parameter, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) >= 90")

  stations <- dbGetQuery(conn, stations_query)

  query <- paste0("SELECT Istasyon_modified, ", parameter, " < ", threshold, " AS ExceedsThreshold
                   FROM daily_detail
                   WHERE Istasyon_modified IN ('", paste(stations$Istasyon_modified, collapse = "','"), "')")

  query_result <- dbGetQuery(conn, query)

  

  disconnect_postgres(conn)

  exceedance_days <- aggregate(ExceedsThreshold ~ Istasyon_modified, query_result, sum)

  exceedance_days %>%
    arrange(desc(ExceedsThreshold))
}
