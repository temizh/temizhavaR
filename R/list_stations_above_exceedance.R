#' Calculate Exceedance Days for specified parameter from daily_detail for all stations
#'
#' This function calculates the number of days exceeding a specified threshold for a given parameter from daily_detail data for all stations.
#'
#' @name calculate_above_exceedance_days_all_stations
#' @param daily_data The daily detail data containing parameter values for each day.
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter.
#' @return A data frame containing the number of days exceeding the specified threshold for the parameter for each station.
#' @export

library(DBI)
calculate_above_exceedance_days_all_stations <- function(parameter, threshold) {

  mydb <- create_postgres_conn()
  if (is.null(mydb)) {
    stop("Unable to connect to database")
  }

  stations_query <- paste0("SELECT Istasyon_modified
                            FROM daily_detail
                            GROUP BY Istasyon_modified
                            HAVING (SUM(CASE WHEN ", parameter, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) >= 90")

  stations <- dbGetQuery(mydb, stations_query)

  query <- paste0("SELECT Istasyon_modified, ", parameter, " > ", threshold, " AS ExceedsThreshold
                   FROM daily_detail
                   WHERE Istasyon_modified IN ('", paste(stations$Istasyon_modified, collapse = "','"), "')")



  # query <- paste0("SELECT Istasyon_modified, ", parameter, " > ", threshold, " AS ExceedsThreshold FROM daily_detail")

  query_result <- dbGetQuery(mydb, query)

  disconnect_postgres(mydb)

  exceedance_days <- aggregate(ExceedsThreshold ~ Istasyon_modified, query_result, sum)

  #high_exceedance_stations <- exceedance_days$Istasyon_modified[exceedance_days$ExceedsThreshold >= consecutive_threshold]

  exceedance_days %>%
    arrange(desc(ExceedsThreshold))
}
