library(RSelenium)
library(netstat)
library(wdman)
library(temizhavaR)
library(stringr)
library(DBI)


if(!exists("get_cached_files")) {
  .file_cache <- new.env(parent = emptyenv())
  
  clear_file_cache <- function() {
    rm(list = ls(.file_cache), envir = .file_cache)
  }
  
  get_cached_files <- function(dir, force_refresh = FALSE) {
    dir_key <- normalizePath(dir, mustWork = FALSE)
    
    if (force_refresh || !exists(dir_key, envir = .file_cache)) {
      if (dir.exists(dir)) {
        .file_cache[[dir_key]] <- list.files(dir)
      } else {
        .file_cache[[dir_key]] <- character(0)
      }
    }
    
    return(.file_cache[[dir_key]])
  }
  
  add_file_to_cache <- function(dir, filename) {
    dir_key <- normalizePath(dir, mustWork = FALSE)
    
    if (!exists(dir_key, envir = .file_cache)) {
      .file_cache[[dir_key]] <- character(0)
    }
    
    existing_files <- .file_cache[[dir_key]]
    if (!filename %in% existing_files) {
      .file_cache[[dir_key]] <- c(existing_files, filename)
    }
  }
  
  batch_download_check <- function(city_dir, stations, data_type, startdate, enddate) {
    existing_files <- get_cached_files(city_dir, force_refresh = TRUE)
    
    start_date <- as.Date(startdate, format="%d.%m.%Y")
    end_date <- as.Date(enddate, format="%d.%m.%Y")
    year_pattern <- paste0(format(start_date, "%Y"), "-", format(end_date, "%Y"))
    
    stations_to_download <- character(0)
    
    for (station in stations) {
      if (data_type == "hourly") {
        required_files <- c(
          paste0(station, "_saatlik_detay_", year_pattern, ".xlsx"),
          paste0(station, "_saatlik_ozet_", year_pattern, ".xlsx")
        )
      } else {
        required_files <- c(
          paste0(station, "_gunluk_detay_", year_pattern, ".xlsx"),
          paste0(station, "_gunluk_ozet_", year_pattern, ".xlsx")
        )
      }
      
      missing_files <- !required_files %in% existing_files
      
      if (any(missing_files)) {
        stations_to_download <- c(stations_to_download, station)
      } else {
        invalid_files <- sapply(required_files, function(file) {
          full_path <- file.path(city_dir, file)
          !file.exists(full_path) || file.info(full_path)$size == 0
        })
        
        if (any(invalid_files)) {
          stations_to_download <- c(stations_to_download, station)
        }
      }
    }
    
    return(stations_to_download)
  }
}

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
  
  detail_pattern <- paste0(istasyon_modified, "_", 
                         ifelse(data_type == "hourly", "saatlik", "gunluk"), 
                         "_detay_", start_year, "-", end_year, ".xlsx")
  
  summary_pattern <- paste0(istasyon_modified, "_", 
                          ifelse(data_type == "hourly", "saatlik", "gunluk"), 
                          "_ozet_", start_year, "-", end_year, ".xlsx")
  
  detail_exists <- length(list.files(city_dir, pattern = detail_pattern)) > 0
  summary_exists <- length(list.files(city_dir, pattern = summary_pattern)) > 0
  
  return(!detail_exists || !summary_exists)
}


download_temizhava_data <- function(mode = "default", one_station = NULL,
                                    startdate = NULL, enddate = NULL, year = NULL, start_year = NULL,
                                    selected_region = NULL) {
  tryCatch({
    clear_file_cache()
  }, error = function(e) {
    cat("Warning: Could not clear file cache:", e$message, "\n")
  })
  
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

  conn <- temizhavaR:::create_postgres_conn()
  if (is.null(conn)) {
    stop("Failed to connect to PostgreSQL database")
  }
  
  location <- dbReadTable(conn, "location")

  if (!is.null(one_station)) {
    location <- location[location$Istasyon_modified == one_station, ]
    if (nrow(location) == 0) {
      stop(sprintf("Station '%s' not found in the database", one_station))
    }
    cat(sprintf("Processing single station: %s\n", one_station))
  }
  
  if (!is.null(selected_region)) {
    location <- location[location$Bolge == selected_region, ]
    if (nrow(location) == 0) {
      stop(sprintf("No stations found for region '%s'", selected_region))
    }
    cat(sprintf("Processing only stations in region: %s (%d stations)\n", 
               selected_region, nrow(location)))
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
                            , extraCapabilities = eCaps, phantomver = NULL)
  
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
          Sys.sleep(5)
          
          if (!wait_for_element(remDr, selector, type = "css", timeout = 5)) {
            return(FALSE)
          }
        }
      }
      
      tryCatch({
        remDr$findElement("id", "dropdown12-contentDataDowloadNew")$clickElement()
        Sys.sleep(2)
        regions <- remDr$findElements("css", ".k-list-container.k-popup li")
        if (length(regions) == 0) {
          cat("Region dropdown is empty\n")
          return(FALSE)
        }
        
        remDr$findElement("xpath", '//*[@id="page-wrapper"]/div[1]')$clickElement()
        Sys.sleep(1)
      }, error = function(e) {
        cat("Error checking region dropdown:", e$message, "\n")
        return(FALSE)
      })
      
      return(TRUE)
    }, error = function(e) {
      cat("Error in verify_page_state:", e$message, "\n")
      return(FALSE)
    })
  }

  ensure_session <- function(remDr) {
    tryCatch({
      remDr$getTitle()
      attempts <- 0
      max_attempts <- 3
      
      while (!verify_page_state(remDr) && attempts < max_attempts) {
        attempts <- attempts + 1
        cat(sprintf("Page not in correct state, attempt %d of %d\n", attempts, max_attempts))
        
        tryCatch({
          reset_btn <- remDr$findElement(using = "css", ".btn.btn-block.btn-xs.btn-primary")
          reset_btn$clickElement()
          Sys.sleep(3)
        }, error = function(e) {
          cat("Could not find reset button, proceeding with page refresh\n")
        })
        
        remDr$refresh()
        Sys.sleep(5)
        
        if (attempts == max_attempts) {
          remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")
          Sys.sleep(5)
        }
      }
      
      if (!verify_page_state(remDr)) {
        stop("Failed to establish a valid page state after multiple attempts")
      }
      
      return(remDr)
    }, error = function(e) {
      cat("Session lost or page error, attempting to reconnect...\n")
      
      tryCatch({
        system("pkill -f 'chrome|chromedriver|selenium'", ignore.stderr = TRUE)
        Sys.sleep(3)  
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
            new_remDr$findElement("id", "dropdown12-contentDataDowloadNew")$clickElement()
            Sys.sleep(2)
            regions <- new_remDr$findElements("css", ".k-list-container.k-popup li")
            if (length(regions) == 0) {
              cat("Region dropdown is empty after reconnection\n")
              new_remDr$close()
              next
            }
            
            new_remDr$findElement("xpath", '//*[@id="page-wrapper"]/div[1]')$clickElement()
            Sys.sleep(1)
            
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

  wait_for_element <- function(remDr, selector, type = "css", timeout = 3, interval = 0.5) {
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
    
    detail_pattern <- paste0(istasyon_modified, "_", 
                              ifelse(data_type == "hourly", "saatlik_detay", "gunluk_detay"), 
                              "_", start_year, "-", end_year, ".xlsx")
    summary_pattern <- paste0(istasyon_modified, "_", 
                               ifelse(data_type == "hourly", "saatlik_ozet", "gunluk_ozet"), 
                               "_", start_year, "-", end_year, ".xlsx")
    
    detail_exists <- length(list.files(city_dir, pattern = detail_pattern)) > 0
    summary_exists <- length(list.files(city_dir, pattern = summary_pattern)) > 0
    
    success <- detail_exists || summary_exists
    
    if (!success) {
      cat(sprintf("\nDownload verification failed for %s - %s\n", istasyon_modified, data_type))
      files <- list.files(city_dir, pattern = paste0(istasyon_modified, "_[^_]+_", start_year, "-", end_year, ".xlsx"))
      cat("Files found:", paste(files, collapse=", "), "\n")
    }
    
    return(success)
  }

  handle_station_download <- function(remDr, station_info, data_type, startdate, enddate, result_dir, max_time = 300) {
    start_time <- Sys.time()
    
    while(difftime(Sys.time(), start_time, units="secs") < max_time) {
      tryCatch({
        tryCatch({
          reset_btn <- remDr$findElement(using = "css", ".btn.btn-block.btn-xs.btn-primary")
          reset_btn$clickElement()
          Sys.sleep(3)
        }, error = function(e) {})
        
        download_data(
          remDr = remDr,
          bolge = station_info$Bolge,
          sehir = station_info$Sehir,
          istasyon = station_info$Istasyon_original,
          data_type = data_type,
          startdate = startdate,
          enddate = enddate,
          result_dir = result_dir
        )
        
        return(NULL)
        
      }, error = function(e) {
        cat(sprintf("Download attempt failed: %s. Retrying...\n", e$message))
        Sys.sleep(5)
        remDr <- ensure_session(remDr)
      })
    }
    
    return(sprintf("Timeout exceeded for %s - %s", station_info$Istasyon_original, data_type))
  }

  all_downloads_completed <- TRUE  
  all_missing_files <- list()

  location_by_city <- split(location, location$Sehir)
  
  failed_stations <- list(
    daily = character(0),
    hourly = character(0)
  )
  skipped_stations <- character(0)

  for (city_name in names(location_by_city)) {
    city_stations <- location_by_city[[city_name]]
    cat("\n--- Processing City:", city_name, "---\n")
    
    city_dir <- file.path(result_dir, city_name)
    if (!dir.exists(city_dir)) {
      cat("Creating directory:", city_dir, "\n")
      dir.create(city_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    city_station_modified <- as.character(city_stations$Istasyon_modified)
    
    daily_downloads_needed <- batch_download_check(
      city_dir, 
      city_station_modified, 
      "daily", 
      startdate, 
      enddate
    )
    
    hourly_downloads_needed <- batch_download_check(
      city_dir, 
      city_station_modified, 
      "hourly", 
      startdate, 
      enddate
    )
    
    for (i in 1:nrow(city_stations)) {
      current_station <- city_stations[i, , drop = FALSE]
      
      cat("\n--- Processing Station:", i, "of", nrow(city_stations), "---\n")
      print(current_station)
      
      bolge <- as.character(current_station$Bolge)
      sehir <- as.character(current_station$Sehir)
      istasyon_original <- as.character(current_station$Istasyon_original)
      istasyon_modified <- as.character(current_station$Istasyon_modified)
        
      cat("Bolge:", bolge, "\n")
      cat("Sehir:", sehir, "\n")
      cat("Istasyon:", istasyon_original, "\n")
      
      if (!(istasyon_modified %in% daily_downloads_needed) && 
          !(istasyon_modified %in% hourly_downloads_needed)) {
        skipped_stations <- c(skipped_stations, 
                             sprintf("%s (%s)", istasyon_original, sehir))
        cat("All files already exist for this station. Skipping.\n")
        next
      }
      
      max_retries <- 3
      for(retry in 1:max_retries) {
        tryCatch({
          remDr <- ensure_session(remDr)
          if (verify_page_state(remDr)) {
            break
          }
        }, error = function(e) {
          cat(sprintf("Retry %d/%d failed: %s\n", retry, max_retries, e$message))
          Sys.sleep(5) 
        })
        
        if (retry == max_retries) {
          cat(sprintf("Failed to establish stable connection for station: %s after %d attempts\n", 
                     istasyon_original, max_retries))
          next
        }
      }
      
      if (istasyon_modified %in% daily_downloads_needed) {
        cat("Downloading daily data for:", istasyon_original, "\n")
        
        error_msg <- handle_station_download(
          remDr = remDr,
          station_info = current_station,
          data_type = "daily",
          startdate = startdate,
          enddate = enddate,
          result_dir = result_dir
        )
        
        if (!is.null(error_msg)) {
          failed_stations$daily <- c(failed_stations$daily, 
                                   sprintf("%s (%s)", istasyon_original, sehir))
          all_missing_files <- c(all_missing_files, error_msg)
          cat("Daily download failed after timeout, skipping to next...\n")
        }
        
        if (check_download_success(city_dir, istasyon_modified, "daily", startdate, enddate)) {
          get_cached_files(city_dir, force_refresh = TRUE)
        }
      }

      if (istasyon_modified %in% hourly_downloads_needed) {
        cat("Downloading hourly data for:", istasyon_original, "\n")
        
        error_msg <- handle_station_download(
          remDr = remDr,
          station_info = current_station,
          data_type = "hourly",
          startdate = startdate,
          enddate = enddate,
          result_dir = result_dir
        )
        
        if (!is.null(error_msg)) {
          failed_stations$hourly <- c(failed_stations$hourly, 
                                    sprintf("%s (%s)", istasyon_original, sehir))
          all_missing_files <- c(all_missing_files, error_msg)
          cat("Hourly download failed after timeout, skipping to next...\n")
        }
        
        if (check_download_success(city_dir, istasyon_modified, "hourly", startdate, enddate)) {
          get_cached_files(city_dir, force_refresh = TRUE)
        }
      }
    }
  }

  if (all_downloads_completed) {
    cat("\nAll station downloads completed successfully!\n")
  } else {
    cat("\nDownloads completed with some errors. Please check the logs above.\n")
  }

  if (length(failed_stations$daily) > 0 || length(failed_stations$hourly) > 0) {
    cat("\n=== Failed Downloads Report ===\n")
    if (length(failed_stations$daily) > 0) {
      cat("\nFailed Daily Downloads:\n")
      cat(paste("-", failed_stations$daily), sep = "\n")
    }
    if (length(failed_stations$hourly) > 0) {
      cat("\nFailed Hourly Downloads:\n")
      cat(paste("-", failed_stations$hourly), sep = "\n")
    }
    cat("\nTotal failed stations:", 
        length(unique(c(failed_stations$daily, failed_stations$hourly))), "\n")
  }

  if (length(skipped_stations) > 0) {
    cat("\n=== Skipped Stations (Already Downloaded) ===\n")
    cat(paste("-", skipped_stations), sep = "\n")
    cat("\nTotal skipped stations:", length(skipped_stations), "\n")
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


# download_temizhava_data(startdate = "01.01.2024", enddate = "01.01.2025")

# download_temizhava_data(mode = "yearly", year = 2023)

 # Decade mode starting from 2010
# download_temizhava_data(mode = "decade", start_year = 2010)

# Example usage
# Download all stations
download_temizhava_data(startdate = "01.01.2014", enddate = "01.01.2025")

# Download only stations in a specific region
# download_temizhava_data(startdate = "01.01.2024", enddate = "01.01.2025", selected_region = "Marmara THM")

# Download a specific year for a specific region
# download_temizhava_data(mode = "yearly", year = 2023, selected_region = "Ege THM")

# download_temizhava_data(one_station = "İstanbul-Alibeyköy", startdate = "01.01.2024", enddate = "01.01.2025")
