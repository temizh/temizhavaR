#' Data Preprocessing
#'
#' @param data station based air parameters data.
#' @export

data_preprocessing <- function(data, station_name) {
  # take station name
  #station_name <- colnames(data)[2]
  #station_name <- iconv(station_name, to = "UTF-8")

  colnames(data) <- data[1, ]
  param_names_with_units <- colnames(data)

  # split param unit from param names
  param_names <- gsub(" \\(.*\\)", "", param_names_with_units)
  param_names <- gsub("\\s+", "", param_names)
  param_units <- gsub(".*\\((.*)\\).*", "\\1", param_names_with_units)

  colnames(data) <- param_names

  colnames(data)[1] <- "Tarih"
  data <- data[-1, ]
  data$Tarih <- gsub("\\.", "\\/", data$Tarih)
  #data$Tarih <- as.POSIXct(data$Tarih)
  data$Tarih <- force_tz(dmy_hms(data$Tarih, tz = "UTC"), tzone = "Turkey")

  suppressWarnings(data <- data %>%
    mutate(across(-1, ~as.numeric(gsub(",", ".", .))))
    )

  #return(data)

  processed_data <- list(data = data, station_name = station_name, param_names = param_names,  param_units = param_units)
  return(processed_data)
}


