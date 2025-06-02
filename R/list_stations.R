#' List stations
#'
#' @param data The data with necessary columns (in tbl format)
#' @param parameter The parameter to calculate the average for
#' @param threshold The threshold for the percentage
#' @export

list_stations <- function(data, parameter, threshold) {
  # Filter the data
  filtered_data <- data %>%
    filter(.data[[paste0(parameter, "_Veri_Mevcudiyeti")]] > threshold) %>%
    rename(result = paste0(parameter, "_Veri_Mevcudiyeti")) %>%
    select(Istasyon, Yıl, result)

  # Widen the data by year
  wide <- filtered_data %>%
    pivot_wider(names_from = Yıl, values_from = result, values_fill = NA)

  # Identify year columns dynamically (assuming years are 4-digit numbers starting with 20)
  year_cols <- colnames(wide) %>%
    grep("^20\\d{2}$", ., value = TRUE) %>%
    sort()

  wide <- wide %>%
    select(Istasyon, all_of(year_cols), everything())  # Order columns

  return(wide)
}