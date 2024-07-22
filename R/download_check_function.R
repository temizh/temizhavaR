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


download_check <- function(city_dir, istasyon, data_type) {
  hourly_detail <- paste0(istasyon, "_saatlik_detay_2023.xlsx")
  hourly_summary <- paste0(istasyon, "_saatlik_ozet_2023.xlsx")
  daily_detail <- paste0(istasyon, "_gunluk_detay_2023.xlsx")
  daily_summary <- paste0(istasyon, "_gunluk_ozet_2023.xlsx")

  required_files <- list(hourly_detail, hourly_summary, daily_detail, daily_summary)
  existing_files <- list.files(city_dir)

  for (file in required_files) {
    if (!(file %in% existing_files)) {
      return(TRUE)
    }
  }

  return(FALSE)
}
