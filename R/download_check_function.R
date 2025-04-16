library(stringr)
library(readxl)

#' Download check
#'
#' @param city_dir The directory where city data is stored.
#' @param istasyon_modified The modified station name (with spaces/dots/slashes removed).
#' @param data_type The type of data to check ("hourly" or "daily").
#' @param startdate The start date for the data (format: "DD.MM.YYYY").
#' @param enddate The end date for the data (format: "DD.MM.YYYY").
#' @return TRUE if files are missing or invalid (need downloading), FALSE if all files exist and are valid.
#' @export

download_check <- function(city_dir, istasyon_modified, data_type, startdate, enddate) {
  # Check if required packages are available
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required but not installed. Please install it with install.packages('readxl')")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed. Please install it with install.packages('stringr')")
  }

  start_date <- as.Date(startdate, format="%d.%m.%Y")
  end_date <- as.Date(enddate, format="%d.%m.%Y")
  
  year_pattern <- paste0(format(start_date, "%Y"), "-", format(end_date, "%Y"))
  
  if (data_type == "hourly") {
    required_files <- c(
      paste0(istasyon_modified, "_saatlik_detay_", year_pattern, ".xlsx"),
      paste0(istasyon_modified, "_saatlik_ozet_", year_pattern, ".xlsx")
    )
  } else if (data_type == "daily") {
    required_files <- c(
      paste0(istasyon_modified, "_gunluk_detay_", year_pattern, ".xlsx"),
      paste0(istasyon_modified, "_gunluk_ozet_", year_pattern, ".xlsx")
    )
  } else {
    stop("Invalid data_type. Must be 'hourly' or 'daily'")
  }
  
  existing_files <- list.files(city_dir)
  
  cat(sprintf("\nChecking %s data files for station: %s\n", data_type, istasyon_modified))
  cat("Required files:", paste(required_files, collapse = ", "), "\n")
  
  all_files_exist <- all(sapply(required_files, function(file) {
    full_path <- file.path(city_dir, file)
    file_exists <- file %in% existing_files
    if (file_exists) {
      file_size <- file.info(full_path)$size
      valid_file <- !is.na(file_size) && file_size > 0
      if (!valid_file) {
        cat(sprintf("File exists but is empty: %s\n", file))
        return(FALSE)
      }
      station_in_file <- NA
      tryCatch({
        if (data_type == "hourly") {
          if (grepl("_detay_", file)) {
            # For saatlik detay, station name is in range B1:D1
            station_in_file <- tryCatch({
              value <- readxl::read_excel(full_path, range = "B1:D1", col_names = FALSE)
              vec <- as.character(value[1,])
              vec[which(nzchar(trimws(vec)))[1]]
            }, error = function(e) NA)
          } else if (grepl("_ozet_", file)) {
            # For saatlik özet, station name is in range A2:L2, and has a prefix "İstasyon:"
            station_in_file <- tryCatch({
              value <- readxl::read_excel(full_path, range = "A2:L2", col_names = FALSE)
              vec <- as.character(value[1,])
              vec <- sub("^İstasyon:\\s*", "", vec)
              vec[which(nzchar(trimws(vec)))[1]]
            }, error = function(e) NA)
          }
        } else if (data_type == "daily") {
          if (grepl("_detay_", file)) {
            # For günlük detay, station name is in cell A2
            station_in_file <- tryCatch({
              value <- readxl::read_excel(full_path, range = "A2", col_names = FALSE)[[1,1]]
              as.character(value)[1]
            }, error = function(e) NA)
          } else if (grepl("_ozet_", file)) {
            # For günlük özet, station name is in range A2:L2, with "İstasyon:" prefix to remove
            station_in_file <- tryCatch({
              value <- readxl::read_excel(full_path, range = "A2:L2", col_names = FALSE)
              vec <- as.character(value[1,])
              vec <- sub("^İstasyon:\\s*", "", vec)
              vec[which(nzchar(trimws(vec)))[1]]
            }, error = function(e) NA)
          }
        }
        if (is.na(station_in_file) || station_in_file == "") {
          cat(sprintf("No station name found in file: %s\n", file))
          return(FALSE)
        }
        station_in_file_modified <- stringr::str_replace_all(station_in_file, 
          c(" " = "", "\\." = "", "/" = "_"))
        if (station_in_file_modified != istasyon_modified) {
          cat(sprintf("File %s has mismatched station name: %s, expected: %s\n",
                      file, station_in_file_modified, istasyon_modified))
          return(FALSE)
        }
      }, error = function(e) {
        cat(sprintf("Error reading Excel file %s: %s\n", file, e$message))
        return(FALSE)
      })
      return(TRUE)
    } else {
      cat(sprintf("Missing file: %s\n", file))
      return(FALSE)
    }
  }))
  
  return(!all_files_exist)
}
