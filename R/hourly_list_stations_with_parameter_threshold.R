#' List Stations for specified parameter from hourly_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @return A data frame with stations and their data availability percentage.
#' @export

hourly_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90) {
  parameter_name <- gsub('\\"', "", parameter_name)
  data <- all_hourly_detail_load_from_database()

  query_result <- data %>%
    group_by(Istasyon_modified) %>%
    summarize(
      total_hours = n(),
      non_na_hours = sum(!is.na(.data[[parameter_name]])),
      veri_mevcudiyet_yuzdesi = round(non_na_hours / 8761 * 100, 2)
    ) %>%
    arrange(desc(veri_mevcudiyet_yuzdesi)) %>%
    mutate(threshold_status = ifelse(veri_mevcudiyet_yuzdesi >= threshold, "Üstünde", "Altında"))

  return(as.data.frame(query_result))
}
