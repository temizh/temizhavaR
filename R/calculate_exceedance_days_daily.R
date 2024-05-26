#' Calculate Exceedance Days for specified parameter from daily_detail
#'
#' This function calculates the number of days exceeding a specified threshold for a given parameter from daily_detail data.
#'
#' @param daily_data The daily detail data containing parameter values for each day.
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter.
#' @return The number of days exceeding the specified threshold for the parameter.
#' @export


calculate_exceedance_days_daily <- function(data, parameter, threshold) {

  if (!(parameter %in% colnames(data))) {
    stop("Belirtilen parametre veri setinde bulunamadı.")
  }

  data$Gun <- as.Date(data$Tarih)

  exceedance_list <- data %>%
    group_by(Istasyon, Gun) %>%
    summarise(daily_mean = mean(get(parameter), na.rm = TRUE)) %>%
    filter(daily_mean > threshold) %>%
    ungroup() %>%
    distinct(Istasyon)

  result_df <- exceedance_list %>% select(Istasyon)

  return(result_df)
}


