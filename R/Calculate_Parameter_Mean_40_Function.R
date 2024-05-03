
#' Calculate specified parameter of specified station with threshold
#'
#' @param station_name The name of the station to filter the data.
#' @export

########databaseden gelsin veriler

get_station_name_mean_above_40 <- function(all_query_result, parameter_name, threshold = 0.9) {
  # query_result'da parametre için ortalama hesapla
  mean_value <- calculate_parameter_mean(query_result, parameter_name, threshold)

  if (!is.na(mean_value) && mean_value > 40) {

    return(query_result$station_name)
  } else {

    return(NULL)
  }
}
