#' Count stations
#'
#' @param data The data with necessary columns (in tbl format)
#' @param parameter The parameter to calculate the average for
#' @param threshold The threshold for the percentage
#' @export

count_stations <- function(data, parameter, threshold) {
  data <- data %>%
    group_by(Yıl) %>%
    summarise(
      Istasyon_Sayısı = sum(ifelse(.data[[paste0(parameter, "_Veri_Mevcudiyeti")]] > threshold, 1, 0))
    ) %>%
    select(Yıl, Istasyon_Sayısı) %>%
    arrange(Yıl)

  return(data)
}