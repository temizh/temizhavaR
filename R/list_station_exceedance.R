#' List station exceedance
#'
#' @param data The data with necessary columns (in tbl format)
#' @param parameter The parameter to calculate the average for
#' @param threshold The threshold for the percentage
#' @param data_threshold The threshold for the percentage
#' @param threshold_direction The direction of the data threshold
#' @param exceedance_threshold The threshold for the exceedance
#' @export

list_station_exceedance <- function(data, parameter, threshold, data_threshold, threshold_direction, exceedance_threshold = 0) {
  column_name <- paste0(parameter, "_", data_threshold, "_", threshold_direction, "_Veri_Sayısı")

  filtered_data <- data %>%
    filter(.data[[paste0(parameter, "_Veri_Mevcudiyeti")]] > threshold) %>%
    filter(.data[[column_name]] > exceedance_threshold) %>%
    rename(result = column_name) %>%
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