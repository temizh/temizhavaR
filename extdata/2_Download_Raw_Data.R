library(RSelenium)
library(netstat)
library(wdman)
library(uuid)
library(RSQLite)
library(temizhavaR)




handle_downloaded_file <- function(download_dir, city_dir, istasyon, data_type, startdate) {
  indirilen_dosyalar <- list.files(download_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  cat("Checking download directory:", download_dir, "\n")
  cat("Number of files found:", length(indirilen_dosyalar), "\n")
  
  if (length(indirilen_dosyalar) == 0) {
    message("No Excel files found in download directory")
    return(FALSE)
  }
  
  mevcut_dosya <- indirilen_dosyalar[length(indirilen_dosyalar)]
  if (is.na(mevcut_dosya) || !nzchar(mevcut_dosya)) {
    message("Invalid filename encountered.")
    return(FALSE)
  }
  cat("Latest downloaded file:", mevcut_dosya, "\n")

  if (file.exists(mevcut_dosya)) {
    year <- format(as.Date(startdate, format = "%d.%m.%Y"), "%Y")
    yeni_dosya_adi <- paste0(istasyon, "_", 
                            if (data_type == "hourly") "saatlik" else "gunluk", 
                            "_", year, ".xlsx")
    yeni_dosya_yolu <- file.path(city_dir, yeni_dosya_adi)

    file.rename(mevcut_dosya, yeni_dosya_yolu)
    message(paste("File successfully moved to:", yeni_dosya_yolu))
    return(TRUE)
  } else {
    message(paste("Download failed or file not found for:", istasyon))
    return(FALSE)
  }
}

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

  

for (i in 1:nrow(location)) {
  current_station <- location[i, , drop = FALSE]
  
  cat("\n--- Processing Row:", i, "---\n")
  print(current_station)
  
  bolge <- as.character(current_station$Bolge)
  sehir <- as.character(current_station$Sehir)
  istasyon <- as.character(current_station$Istasyonlar)
  
  cat("Bolge:", bolge, "\n")
  cat("Sehir:", sehir, "\n")
  cat("Istasyon:", istasyon, "\n")
  
  city_dir <- file.path(result_dir, sehir)
  if (!dir.exists(city_dir)) {
    cat("Creating directory:", city_dir, "\n")
    dir.create(city_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (download_check(city_dir, istasyon, "daily", startdate, enddate)) {
    cat("Downloading daily data for:", istasyon, "\n")
    download_data(
      remDr = remDr,
      bolge = bolge,
      sehir = sehir,
      istasyon = istasyon,
      data_type = "daily",
      startdate = startdate,
      enddate = enddate,
      result_dir = result_dir
    )
    Sys.sleep(12)  
    handle_downloaded_file(result_dir, city_dir, istasyon, "daily", startdate)
  }
  
  if (download_check(city_dir, istasyon, "hourly", startdate, enddate)) {
    cat("Downloading hourly data for:", istasyon, "\n")
    download_data(
      remDr = remDr,
      bolge = bolge,
      sehir = sehir,
      istasyon = istasyon,
      data_type = "hourly",
      startdate = startdate,
      enddate = enddate,
      result_dir = result_dir
    )
    Sys.sleep(12) 
    handle_downloaded_file(result_dir, city_dir, istasyon, "hourly", startdate)
  }
}


  remDr$close()
  remote_driver$server$stop()

  dbDisconnect(mydb)
}

# Default mode with specific start and end dates

download_temizhava_data(startdate = "01.01.2014", enddate = "01.01.2024")

# Yearly mode for 2023
# download_temizhava_data(mode = "yearly", year = 2023)

# # Decade mode starting from 2010
# download_temizhava_data(mode = "decade", start_year = 2010)
