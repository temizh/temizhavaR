library(RSelenium)
library(netstat)
library(wdman)
library(uuid)
library(RSQLite)
library(temizhavaR)
library(stringr)



download_temizhava_data <- function(mode = "default", 
                                    startdate = NULL, enddate = NULL, year = NULL, start_year = NULL) {
  
  if (mode == "default" && (is.null(startdate) || is.null(enddate))) {
    stop("For 'default' mode, 'startdate' and 'enddate' must be provided.")
  }
  
  if (mode == "yearly" && is.null(year)) {
    stop("For 'yearly' mode, 'year' must be provided.")
  }

  if (mode == "decade" && is.null(start_year)) {
    stop("For 'decade' mode, 'start_year' must be provided.")
  }

  if (mode == "yearly") {
    startdate <- paste0("01.01.", year)
    enddate <- paste0("01.01.", year + 1)
  } else if (mode == "decade") {
    startdate <- paste0("01.01.", start_year)
    enddate <- paste0("01.01.", start_year + 10)
  }
  result_dir <- file.path(getwd(), "TemizHava_raw_data")
  if (!dir.exists(result_dir)) {
    dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  }


  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
  location <- dbReadTable(mydb, "location")

  if (nrow(location) == 0) {
    print("There are no stations to download data from.")
    dbDisconnect(mydb)
    return()
  }

  eCaps <- list(chromeOptions = list(prefs = list(
    "download.default_directory" = normalizePath(result_dir),
    "download.prompt_for_download" = FALSE,
    "download.directory_upgrade" = TRUE,
    "safebrowsing.enabled" = TRUE
  )))

  # Server setup
 
  remote_driver <- rsDriver(browser = "chrome", port = 4445L, chromever = "latest", verbose = FALSE
                            , extraCapabilities = eCaps)
  
  remDr <- remote_driver$client
  remDr$maxWindowSize()

  remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")
  Sys.sleep(5)

  all_downloads_completed <- TRUE  

for (i in 1:nrow(location)) {
  current_station <- location[i, , drop = FALSE]
  
  cat("\n--- Processing Row:", i, "---\n")
  print(current_station)
  
  bolge <- as.character(current_station$Bolge)
  sehir <- as.character(current_station$Sehir)
  istasyon_original <- as.character(current_station$Istasyonlar)
    # Replace slashes  with _, and remove spaces and  dots from the station name
  istasyon_modified  <- str_replace_all(istasyon_original, c(" " = "", "\\." = "", "/" = "_"))

    
  cat("Bolge:", bolge, "\n")
  cat("Sehir:", sehir, "\n")
  cat("Istasyon:", istasyon_original, "\n")
  
  city_dir <- file.path(result_dir, sehir)
  if (!dir.exists(city_dir)) {
    cat("Creating directory:", city_dir, "\n")
    dir.create(city_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  tryCatch({
    cat("Checking existing files for:", istasyon_original, "\n")
    
    if (download_check(city_dir, istasyon_modified, "daily", startdate, enddate)) {
      cat("Downloading daily data for:", istasyon_original, "\n")
      download_data(
        remDr = remDr,
        bolge = bolge,
        sehir = sehir,
        istasyon = istasyon_original,
        data_type = "daily",
        startdate = startdate,
        enddate = enddate,
        result_dir = result_dir
      )
      Sys.sleep(12)  
    } else {
      cat("Skipping daily data download - file already exists\n")
    }
    
    if (download_check(city_dir, istasyon_modified, "hourly", startdate, enddate)) {
      cat("Downloading hourly data for:", istasyon_original, "\n")
      download_data(
        remDr = remDr,
        bolge = bolge,
        sehir = sehir,
        istasyon = istasyon_original,
        data_type = "hourly",
        startdate = startdate,
        enddate = enddate,
        result_dir = result_dir
      )
      Sys.sleep(12) 
    } else {
      cat("Skipping hourly data download - file already exists\n")
    }
  }, error = function(e) {
    cat("Error downloading data for station:", istasyon_original, "\n")
    cat("Error message:", e$message, "\n")
    all_downloads_completed <- FALSE
  })
}

if (all_downloads_completed) {
  cat("\nAll station downloads completed successfully!\n")
} else {
  cat("\nDownloads completed with some errors. Please check the logs above.\n")
}

remDr$close()
remote_driver$server$stop()

dbDisconnect(mydb)
cat("\nDatabase connection closed.\n")

}

# Default mode with specific start and end dates

download_temizhava_data(startdate = "01.01.2014", enddate = "01.01.2024")

# Yearly mode for 2023
# download_temizhava_data(mode = "yearly", year = 2023)

# # Decade mode starting from 2010
# download_temizhava_data(mode = "decade", start_year = 2010)
