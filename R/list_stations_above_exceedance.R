#' Calculate Exceedance Days for specified parameter from daily_detail for all stations
#'
#' This function calculates the number of days exceeding a specified threshold for a given parameter from daily_detail data for all stations.
#'
#' @param daily_data The daily detail data containing parameter values for each day.
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter.
#' @return A data frame containing the number of days exceeding the specified threshold for the parameter for each station.
#' @export

calculate_above_exceedance_days_all_stations <- function(parameter, threshold = 50, consecutive_threshold = 35) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT Istasyon, ", parameter, " > ", threshold, " AS ExceedsThreshold FROM daily_detail")

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  exceedance_days <- aggregate(ExceedsThreshold ~ Istasyon, query_result, sum)

  high_exceedance_stations <- exceedance_days$Istasyon[exceedance_days$ExceedsThreshold >= consecutive_threshold]

  return(list(ExceedanceDays = exceedance_days, HighExceedanceStations = high_exceedance_stations))
}
