#' List Stations with threshold
#'
#' @param processed_data A list containing processed data.
#' @param param_name The name of the parameter to check.
#' @export
list_stations_above_threshold <- function(processed_data, param_name) {
  data <- processed_data$data
  station_name <- processed_data$station_name
  param_names <- processed_data$param_names

  if (!is.null(station_name)) {
    if (param_name %in% param_names) {
      if (param_name %in% colnames(data)) {
        param_data <- data[[param_name]]


        if (mean(!is.na(param_data)) >= 0.9) {
          cat("İstenilen parametre için yüzde 90 ve üstü veri alan istasyonlar:\n")
          cat(station_name, "\n")
          return(station_name)
        } else {
          cat("Belirtilen parametre için yüzde 90 ve üstü veri alan herhangi bir istasyon bulunamadi.\n")
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
