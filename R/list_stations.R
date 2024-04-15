#' List Stations
#'
#' @param data A pair of numbers.
#' @export




list_stations <- function(processed_data, param_name) {
  data <- processed_data$data
  station_name <- processed_data$station_name
  param_names <- processed_data$param_names

  if (!is.null(station_name)) {
    if (param_name %in% param_names) {
      if (param_name %in% colnames(data)) {
        if (any(!is.na(data[[param_name]]))) {
          cat(paste0(param_name, "  istasyonlar:\n"))
          cat(station_name, "\n")
        } else {
          cat("Belirtilen parametre icin herhangi bir değer iceren istasyon bulunamadi.\n")
        }
      } else {
        cat(paste0(param_name, " parametresi bulunamadi.\n"))
      }
    } else {
      cat(paste0(param_name, " parametresi gecersiz.\n"))
    }
  } else {
    cat("İstasyon adlari bulunamadi.\n")
  }
}












































