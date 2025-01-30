#' Download check
#'
#' @param bolge The region to select.
#' @param sehir The city to select.
#' @param istasyon The station to select.
#' @param data_type The type of data to download ("hourly" or "daily").
#' @param startdate The start date for the data download (format: "DD.MM.YYYY").
#' @param enddate The end date for the data download (format: "DD.MM.YYYY").
#' @param result_dir The directory where the downloaded data will be saved.
#' @return None. Downloads the data and saves it to the specified directory.
#' @export


download_check <- function(city_dir, istasyon, data_type, startdate, enddate) {
  startYear <- format(as.Date(startdate, "%d.%m.%Y"), "%Y")
  endYear <- format(as.Date(enddate, "%d.%m.%Y"), "%Y")
  
  hourly_detail <- paste0(istasyon, "_saatlik_detay_", startYear, "-", endYear, ".xlsx")
  hourly_summary <- paste0(istasyon, "_saatlik_ozet_", startYear, "-", endYear, ".xlsx")
  daily_detail <- paste0(istasyon, "_gunluk_detay_", startYear, "-", endYear, ".xlsx")
  daily_summary <- paste0(istasyon, "_gunluk_ozet_", startYear, "-", endYear, ".xlsx")

  required_files <- list(hourly_detail, hourly_summary, daily_detail, daily_summary)
  existing_files <- list.files(city_dir)
  
  # Debugging logs
  # cat("Checking files in directory:", city_dir, "\n")
  # cat("Required files:", paste(required_files, collapse = ", "), "\n")
  # cat("Existing files:", paste(existing_files, collapse = ", "), "\n")

  for (file in required_files) {
    if (!(file %in% existing_files)) {
      cat("File not found:", file, "\n")
      return(TRUE)  
    }
  }
  return(FALSE) 
}
