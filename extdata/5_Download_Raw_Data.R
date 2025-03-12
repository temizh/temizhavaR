library(RSelenium)
library(netstat)
library(wdman)
library(temizhavaR)
library(stringr)
library(DBI)


#' Check if files already exist for a station
#'
#' @param city_dir Directory where city data is stored
#' @param istasyon_modified Modified station name 
#' @param data_type "daily" or "hourly"
#' @param startdate Start date in DD.MM.YYYY format
#' @param enddate End date in DD.MM.YYYY format
#' @return Boolean indicating if download is needed
download_check <- function(city_dir, istasyon_modified, data_type, startdate, enddate) {
  start_year <- format(as.Date(startdate, format = "%d.%m.%Y"), "%Y")
  end_year <- format(as.Date(enddate, format = "%d.%m.%Y"), "%Y")
  
  # Create patterns for both detail and summary files
  detail_pattern <- paste0(istasyon_modified, "_", 
                         ifelse(data_type == "hourly", "saatlik", "gunluk"), 
                         "_detay_", start_year, "-", end_year, ".xlsx")
  
  summary_pattern <- paste0(istasyon_modified, "_", 
                          ifelse(data_type == "hourly", "saatlik", "gunluk"), 
                          "_ozet_", start_year, "-", end_year, ".xlsx")
  
  # Check if both files exist
  detail_exists <- length(list.files(city_dir, pattern = detail_pattern)) > 0
  summary_exists <- length(list.files(city_dir, pattern = summary_pattern)) > 0
  
  # Return TRUE if either file is missing (download needed)
  return(!detail_exists || !summary_exists)
}


download_temizhava_data <- function(mode = "default", one_station = NULL,
                                    startdate = NULL, enddate = NULL, year = NULL, start_year = NULL) {
  # Clear file cache at start of function
  assign("cached_files", new.env(), envir = .GlobalEnv)
  
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

  result_dir <- getOption("temizhavaR.base_dir")
    
  if (!dir.exists(result_dir)) {
    dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  }

  conn <- create_postgres_conn()
  if (is.null(conn)) {
    stop("Failed to connect to PostgreSQL database")
  }
  
  location <- dbReadTable(conn, "location")

  # Filter for single station if specified
  if (!is.null(one_station)) {
    location <- location[location$Istasyon_modified == one_station, ]
    if (nrow(location) == 0) {
      stop(sprintf("Station '%s' not found in the database", one_station))
    }
    cat(sprintf("Processing single station: %s\n", one_station))
  }

  if (nrow(location) == 0) {
    print("There are no stations to download data from.")
    disconnect_postgres(conn)
    return()
  }

  eCaps <- list(chromeOptions = list(prefs = list(
    "download.default_directory" = normalizePath(result_dir),
    "download.prompt_for_download" = FALSE,
    "download.directory_upgrade" = TRUE,
    "safebrowsing.enabled" = TRUE
  )))

 
  remote_driver <- rsDriver(browser = "chrome", port = 4445L, chromever = NULL, verbose = FALSE
                            , extraCapabilities = eCaps)
  
  remDr <- remote_driver$client
  remDr$maxWindowSize()

  remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")
  Sys.sleep(5)

  verify_page_state <- function(remDr) {
    tryCatch({
      main_elements <- c(
        "#dropdown12-contentDataDowloadNew",  
        "#dropdown1-contentDataDowloadNew", 
        "#dropdown2-contentDataDowloadNew",   
        "#dropdown3-contentDataDowloadNew",  
        "#dropdown4-contentDataDowloadNew"   
      )
      
      for (selector in main_elements) {
        if (!wait_for_element(remDr, selector, type = "css", timeout = 5)) {
          cat(sprintf("Failed to find element: %s\n", selector))
          remDr$refresh()
          Sys.sleep(2)
          return(FALSE)
        }
      }
      
      input_elements <- c(
        "StationDataDownload_StartDateTime",
        "StationDataDownload_EndDateTime"
      )
      
      for (id in input_elements) {
        if (!wait_for_element(remDr, paste0("#", id), type = "css", timeout = 5)) {
          cat(sprintf("Failed to find input: %s\n", id))
          return(FALSE)
        }
      }
      
      return(TRUE)
    }, error = function(e) {
      cat("Error in verify_page_state:", e$message, "\n")
      return(FALSE)
    })
  }

  ensure_session <- function(remDr) {
    tryCatch({
      remDr$getTitle()
      if (!verify_page_state(remDr)) {
        stop("Page not in correct state")
      }
      return(remDr)
    }, error = function(e) {
      cat("Session lost or page error, attempting to reconnect...\n")
      
      tryCatch({
        system("pkill -f 'chrome|chromedriver|selenium'", ignore.stderr = TRUE)
        Sys.sleep(2)  
      }, error = function(e) {
        cat("Failed to kill existing processes, but continuing...\n")
      })
      
      for(port in c(4445L, 4446L, 4447L, 4448L)) {
        tryCatch({
          remote_driver <- rsDriver(browser = "chrome", 
                                  port = port, 
                                  chromever = NULL, 
                                  verbose = FALSE,
                                  extraCapabilities = eCaps)
          new_remDr <- remote_driver$client
          new_remDr$maxWindowSize()
          new_remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")
          cat(sprintf("Successfully reconnected on port %d\n", port))
          Sys.sleep(3)
          if (verify_page_state(new_remDr)) {
            return(new_remDr)
          } else {
            cat("Page not in correct state, retrying...\n")
            new_remDr$close()
          }
        }, error = function(e) {
          cat(sprintf("Failed to connect on port %d, trying next...\n", port))
        })
      }
      stop("Failed to reconnect after trying multiple ports")
    })
  }

  wait_for_element <- function(remDr, selector, type = "css", timeout = 10, interval = 0.5) {
    start_time <- Sys.time()
    while(difftime(Sys.time(), start_time, units="secs") < timeout) {
      elements <- tryCatch({
        if(type == "css") {
          remDr$findElements("css selector", selector)
        } else {
          remDr$findElements("xpath", selector)
        }
      }, error = function(e) list())
      
      if(length(elements) > 0) {
        return(TRUE)
      }
      Sys.sleep(interval)
    }
    return(FALSE)
  }

 
  check_download_success <- function(city_dir, istasyon_modified, data_type, startdate, enddate) {
    start_year <- sub(".*\\.", "", startdate)
    end_year <- sub(".*\\.", "", enddate)
    
    base_pattern <- if(data_type == "daily") {
      paste0(istasyon_modified, "_gunluk_[^_]+_", start_year, "-", end_year, ".xlsx")
    } else {
      paste0(istasyon_modified, "_saatlik_[^_]+_", start_year, "-", end_year, ".xlsx")
    }
    
    files <- list.files(city_dir, pattern = base_pattern)
    return(length(files) >= 2)  
  }

  

  all_downloads_completed <- TRUE  
  all_missing_files <- list()

for (i in 1:nrow(location)) {
  current_station <- location[i, , drop = FALSE]
  
  cat("\n--- Processing Row:", i, "---\n")
  print(current_station)
  
  bolge <- as.character(current_station$Bolge)
  sehir <- as.character(current_station$Sehir)
  istasyon_original <- as.character(current_station$Istasyon_original)
  istasyon_modified <- as.character(current_station$Istasyon_modified)
    
  cat("Bolge:", bolge, "\n")
  cat("Sehir:", sehir, "\n")
  cat("Istasyon:", istasyon_original, "\n")

  max_retries <- 3
  for(retry in 1:max_retries) {
    remDr <- ensure_session(remDr)
    if (verify_page_state(remDr)) {
      break
    }
    if (retry == max_retries) {
      cat(sprintf("Failed to establish stable connection for station: %s after %d attempts\n", 
                 istasyon_original, max_retries))
      next
    }
    Sys.sleep(2)
  }

  
  city_dir <- file.path(result_dir, sehir)
  if (!dir.exists(city_dir)) {
    cat("Creating directory:", city_dir, "\n")
    dir.create(city_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  tryCatch({
    cat("Checking existing files for:", istasyon_original, "\n")
    
    if (download_check(city_dir, istasyon_modified, "daily", startdate, enddate)) {
      cat("Downloading daily data for:", istasyon_original, "\n")
      
    
     
      
      missing_files <- tryCatch({
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
        
        if (!check_download_success(city_dir, istasyon_modified, "daily", startdate, enddate)) {
          c(paste0("Failed to download daily files for: ", istasyon_original))
        } else {
          NULL
        }
      }, error = function(e) {
        cat("Error in daily download:", e$message, "\n")
        c(paste0("Error downloading daily files for: ", istasyon_original))
      })
      
      if (!is.null(missing_files)) {
        all_missing_files <- c(all_missing_files, missing_files)
      }
      Sys.sleep(3) 
    } else {
      cat("Skipping daily data download - file already exists\n")
    }
    
    if (download_check(city_dir, istasyon_modified, "hourly", startdate, enddate)) {
      cat("Downloading hourly data for:", istasyon_original, "\n")
      missing_files <- download_data(
        remDr = remDr,
        bolge = bolge,
        sehir = sehir,
        istasyon = istasyon_original,
        data_type = "hourly",
        startdate = startdate,
        enddate = enddate,
        result_dir = result_dir
      )
      
      if (!check_download_success(city_dir, istasyon_modified, "hourly", startdate, enddate)) {
        stop("Download verification failed - files not found or empty")
      }
      
      all_missing_files <- c(all_missing_files, missing_files)
      Sys.sleep(3) 
    } else {
      cat("Skipping hourly data download - file already exists\n")
    }
  }, error = function(e) {
    cat("Error downloading data for station:", istasyon_original, "\n")
    cat("Error message:", e$message, "\n")
    all_downloads_completed <- FALSE
    
    remDr <- ensure_session(remDr)
  })
  
  Sys.sleep(2)
}

if (all_downloads_completed) {
  cat("\nAll station downloads completed successfully!\n")
} else {
  cat("\nDownloads completed with some errors. Please check the logs above.\n")
}

if (length(all_missing_files) > 0) {
  cat("\nMissing files report:\n")
  for (missing_file in all_missing_files) {
    cat(missing_file, "\n")
  }
}

disconnect_postgres(conn)
cat("\nDatabase connection closed.\n")

}


download_temizhava_data(startdate = "01.01.2024", enddate = "01.01.2025")

# download_temizhava_data(mode = "yearly", year = 2023)

 # Decade mode starting from 2010
# download_temizhava_data(mode = "decade", start_year = 2010)
