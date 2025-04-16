#' List station averages
#'
#' @param data The data with necessary columns (in tbl format)
#' @param parameter The parameter to calculate the average for
#' @param data_threshold The threshold for the percentage
#' @param threshold_direction The direction of the data threshold
#' @export

list_station_averages <- function(data, parameter, data_threshold, threshold_direction, threshold = 0, second_threshold = 0) {
  # Filter the data
  if(threshold_direction == "Üstü") {
    filtered_data <- data %>%
      filter(.data[[paste0(parameter, "_Ortalaması")]] > data_threshold)
  } else if (threshold_direction == "Altı") {
    filtered_data <- data %>%
      filter(.data[[paste0(parameter, "_Ortalaması")]] < data_threshold)
  } else {
    stop("threshold_direction must be either 'Üstü' or 'Altı'")
  }

  if(second_threshold > 0) {
    filtered_data <- filtered_data %>%
      filter(.data[[paste0(parameter, "_Veri_Mevcudiyeti")]] > threshold | .data[[paste0(parameter, "_Ikincil_Veri_Mevcudiyeti")]] > second_threshold)
  }
  else if (threshold > 0) {
    filtered_data <- filtered_data %>%
      filter(.data[[paste0(parameter, "_Veri_Mevcudiyeti")]] > threshold)
  }

  filtered_data <- filtered_data %>%
    rename(result = paste0(parameter, "_Ortalaması")) %>%
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