#' Calculate the rolling exceedance of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the exceedance for
#' @param threshold The threshold value to compare the data with
#' @param rolling_hours The number of hours to calculate the rolling exceedance for
#' @param period The period to calculate the rolling exceedance for (Day, Week, Month, Year)
#' @export

calculate_rolling_exceedance <- function(data, parameter, threshold, rolling_hours, period = "Day", months = NULL) {

  if(!is.null(months)) {
    data <- data %>%
      filter(Ay %in% months)
  }

  print("C")

  data <- data %>%
    distinct(Istasyon, Yıl, Ay, Gün, Tarih, .keep_all = TRUE) %>%
    mutate(
        tmp_date = ifelse(Saat == 0, Tarih - days(1), Tarih),
        Gün = day(tmp_date),
        Ay = month(tmp_date),
        Yıl = year(tmp_date)
    ) %>%
    mutate (
      avg = sql(paste0('AVG("O3") OVER (PARTITION BY "Istasyon" ORDER BY "Tarih" ROWS BETWEEN ', (rolling_hours - 1), ' PRECEDING AND CURRENT ROW)'))
    ) %>%
    group_by(Istasyon, Yıl, Ay, Gün) %>%
    summarise(max = max(avg, na.rm = TRUE), .groups = "drop") %>%
    filter(max > threshold) %>%
    group_by(Istasyon, Yıl) %>%
    summarise(result = n(), .groups = "drop")

  print("D")

  return(data)
}