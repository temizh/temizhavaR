#' Calculate the rolling exceedance of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the exceedance for
#' @param threshold The threshold value to compare the data with
#' @param rolling_hours The number of hours to calculate the rolling exceedance for
#' @param period The period to calculate the rolling exceedance for (Day, Week, Month, Year)
#' @export

calculate_rolling_exceedance <- function(data, parameter, threshold, rolling_hours, con, period = "Day", months = NULL) {

  if(!is.null(months)) {
    data <- data %>%
      filter(Ay %in% months)
  }

  # data <- data %>%
  #   mutate(date = ifelse(period == "Day", paste0(year(Tarih), "-", month(Tarih), "-", day(Tarih)), paste0(year(Tarih), "-", month(Tarih)))) %>%
  #   mutate(rolling_avg = sql(
  #     paste0('AVG("O3") OVER (PARTITION BY "Istasyon", "Yıl" ORDER BY "Tarih" ROWS BETWEEN ', (rolling_hours - 1), ' PRECEDING AND CURRENT ROW)')
  #   )) %>%
  #   group_by(date, .add = TRUE) %>%
  #   summarise(max_avg = max(rolling_avg, na.rm = TRUE), .groups = "drop_last") %>%
  #   mutate(result = sum(ifelse(max_avg > threshold, 1, 0), na.rm = TRUE))

  data <- data %>%
    ungroup() %>%
    distinct(Istasyon, Yıl, Ay, Gün, Tarih, .keep_all = TRUE) %>%
    mutate (
      avg_8hr = sql(paste0('AVG("O3") OVER (PARTITION BY "Istasyon" ORDER BY "Tarih" ROWS BETWEEN ', (rolling_hours - 1), ' PRECEDING AND CURRENT ROW)'))
    ) %>%
    group_by(Istasyon, Yıl, Ay, Gün) %>%
    summarise(max_8hr = max(avg_8hr, na.rm = TRUE), .groups = "drop") %>%
    filter(max_8hr > 120) %>%
    group_by(Istasyon, Yıl) %>%
    summarise(result = n(), .groups = "drop")

  return(data)
}