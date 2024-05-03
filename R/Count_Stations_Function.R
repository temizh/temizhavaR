#' Count Station List
#'
#' @param processed_data Processed data containing station information.
#' @param param_name Name of the parameter to check.
#' @return Number of stations for the specified parameter.
#' @export

count_station_list <- function(processed_data, param_name) {
  data <- processed_data$data
  station_name <- processed_data$station_name
  param_names <- processed_data$param_names

  if (!is.null(station_name)) {
    if (param_name %in% param_names) {
      if (param_name %in% colnames(data)) {
        if (any(!is.na(data[[param_name]]))) {
          # Benzersiz istasyon adlarının sayısını bul
          num_stations <- length(unique(station_name))
          cat(paste0(param_name, " için istasyon sayısı: ", num_stations, "\n"))
          return(num_stations)
        } else {
          cat("Belirtilen parametre için herhangi bir değer içeren istasyon bulunamadı.\n")
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
