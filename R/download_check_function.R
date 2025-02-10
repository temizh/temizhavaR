library(stringr)


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
  modified_istasyon <- str_replace_all(istasyon, c(" " = "", "\\." = "", "/" = "_"))
  start_date <- as.Date(startdate, format="%d.%m.%Y")
  end_date <- as.Date(enddate, format="%d.%m.%Y")
  
  year_pattern <- paste0(format(start_date, "%Y"), "-", format(end_date, "%Y"))
  
  if (data_type == "hourly") {
    required_files <- c(
      paste0(modified_istasyon, "_saatlik_detay_", year_pattern, ".xlsx"),
      paste0(modified_istasyon, "_saatlik_ozet_", year_pattern, ".xlsx")
    )
  } else if (data_type == "daily") {
    required_files <- c(
      paste0(modified_istasyon, "_gunluk_detay_", year_pattern, ".xlsx"),
      paste0(modified_istasyon, "_gunluk_ozet_", year_pattern, ".xlsx")
    )
  } else {
    stop("Invalid data_type. Must be 'hourly' or 'daily'")
  }
  
  # Get existing files
  existing_files <- list.files(city_dir)
  
  cat(sprintf("\nChecking %s data files for station: %s\n", data_type, istasyon))
  cat("Required files:", paste(required_files, collapse = ", "), "\n")
  
  all_files_exist <- all(sapply(required_files, function(file) {
    full_path <- file.path(city_dir, file)
    file_exists <- file %in% existing_files
    if (file_exists) {
      file_size <- file.info(full_path)$size
      valid_file <- !is.na(file_size) && file_size > 0
      if (!valid_file) {
        cat(sprintf("File exists but is empty: %s\n", file))
      }
      return(valid_file)
    } else {
      cat(sprintf("Missing file: %s\n", file))
      return(FALSE)
    }
  }))
  
  return(!all_files_exist)
}
