#' Calculate Exceedance Days for Specified Parameter from Daily Data
#'
#' This function calculates the number of times stations exceed a specified threshold
#' for a given parameter from daily_detail data.
#'
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter. Default is 125.
#' @param exceedance_count The minimum number of times the threshold should be exceeded to be included in the result. Default is 3.
#' @return A data frame containing stations and the number of times they exceed the specified threshold for the parameter.
#' @export

calculate_above_exceedance_days_all_stations <- function(parameter, threshold = 125, exceedance_count = 3) {


  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")


  daily_data <- dbReadTable(mydb, "daily_detail")


  dbDisconnect(mydb)

  # Calculate exceedance days for each station
  exceedance_days <- daily_data %>%
    filter(!is.na(.data[[parameter]])) %>%
    mutate(ExceedsThreshold = .data[[parameter]] > threshold) %>%
    group_by(Istasyon) %>%
    summarise(ExceedanceCount = sum(ExceedsThreshold, na.rm = TRUE)) %>%
    filter(ExceedanceCount > exceedance_count) %>%
    as.data.frame()

  return(exceedance_days)
}

