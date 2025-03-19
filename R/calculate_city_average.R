#' Calculate the city average of available data
#'
#' @param averages A grouped data frame containing the averages (in tbl format)
#' @param parameter The parameter to calculate the average for
#' @param percentages A data frame containing the percentages (in tbl format)
#' @param stations A data frame containing the stations (in tbl format)
#' @export


calculate_city_average <- function(averages, parameter, percentages, stations, threshold) {
  data <- averages %>%
    left_join(percentages, by = c("Istasyon", "Yıl")) %>%
    left_join(stations, by = "Istasyon") %>%
    filter(.data[[paste0(parameter, "_Veri_Mevcudiyeti")]] > threshold) %>%
    group_by(Yıl, Sehir) %>%
    summarise(
      result = mean(.data[[paste0(parameter, "_Ortalaması")]], na.rm = TRUE)  # Calculate average
    )

  return(data)
}