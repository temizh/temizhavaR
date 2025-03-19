#' Calculate the city average of available data
#'
#' @param data The data with necessary columns (in tbl format)
#' @param parameter The parameter to calculate the average for
#' @param cities The cities data
#' @param threshold The threshold for the percentage
#' @export


calculate_city_average <- function(data, parameter, cities, threshold) {
  data <- data %>%
    left_join(cities, by = "Istasyon") %>%
    filter(.data[[paste0(parameter, "_Veri_Mevcudiyeti")]] > threshold) %>%
    select(Sehir, Yıl, .data[[paste0(parameter, "_Ortalaması")]])

  data <- data %>%
    group_by(Sehir, Yıl) %>%
    summarise(
      result = mean(.data[[paste0(parameter, "_Ortalaması")]], na.rm = TRUE)
    ) %>%
    select(Sehir, Yıl, result)

  # widen the data by year
  wide <- data %>%
    pivot_wider(names_from = Yıl, values_from = result, values_fill = NA)

  # Identify year columns dynamically (assuming years are 4-digit numbers starting with 20)
  year_cols <- colnames(wide) %>%
    grep("^20\\d{2}$", ., value = TRUE) %>%
    sort()

  wide <- wide %>%
    select(Sehir, all_of(year_cols), everything())  # Order columns

  return(wide)
}