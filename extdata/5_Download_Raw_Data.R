library(RSelenium)
library(netstat)
library(wdman)
library(temizhavaR)
library(stringr)
library(DBI)


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

  xlsx_is_readable <- function(path) {
    if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
      return(FALSE)
    }
    tryCatch({
      members <- utils::unzip(path, list = TRUE)$Name
      any(grepl("^xl/worksheets/sheet[0-9]+\\.xml$", members)) &&
        "[Content_Types].xml" %in% members
    }, error = function(e) FALSE)
  }
  
batch_download_check <- function(city_dir, stations, data_type, startdate, enddate,
                                 recheck_no_data = FALSE) {
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

      no_data_marker <- paste0(
        station, "_", if (data_type == "hourly") "saatlik" else "gunluk",
        "_no_data_", year_pattern, ".txt"
      )
      if (no_data_marker %in% existing_files && !recheck_no_data) {
        next
      }
      
      missing_files <- !required_files %in% existing_files
      
      if (any(missing_files)) {
        stations_to_download <- c(stations_to_download, station)
      } else {
        invalid_files <- vapply(required_files, function(file) {
          !xlsx_is_readable(file.path(city_dir, file))
        }, logical(1))
        
        if (any(invalid_files)) {
          stations_to_download <- c(stations_to_download, station)
        }
      }
    }
    
  return(stations_to_download)
}


download_temizhava_data <- function(mode = "default", one_station = NULL,
                                    startdate = NULL, enddate = NULL, year = NULL, start_year = NULL,
                                    selected_region = NULL, recheck_no_data = FALSE) {
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
    if (length(year) != 1 || is.na(year) || year < 2000 || year > as.integer(format(Sys.Date(), "%Y"))) {
      stop("'year' must be one valid year between 2000 and the current year.")
    }
    startdate <- paste0("01.01.", year)
    enddate <- paste0("01.01.", year + 1)
  } else if (mode == "decade") {
    startdate <- paste0("01.01.", start_year)
    enddate <- paste0("01.01.", start_year + 10)
  }

  parsed_start <- as.Date(startdate, format = "%d.%m.%Y")
  parsed_end <- as.Date(enddate, format = "%d.%m.%Y")
  if (is.na(parsed_start) || is.na(parsed_end) || parsed_start >= parsed_end) {
    stop("Invalid date range. Use DD.MM.YYYY and ensure startdate is before enddate.")
  }

  result_dir <- getOption("temizhavaR.base_dir")
  if (is.null(result_dir) || length(result_dir) != 1 || !nzchar(result_dir)) {
    stop("Set options(temizhavaR.base_dir = '/absolute/data/directory') before downloading.")
  }

  if (!dir.exists(result_dir)) {
    dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  }

  conn <- temizhavaR:::create_postgres_conn()
  if (is.null(conn)) {
    stop("Failed to connect to PostgreSQL database")
  }
  on.exit(disconnect_postgres(conn), add = TRUE)
  
  location <- dbReadTable(conn, "location")[, c(
    "Bolge", "Sehir", "Istasyon_original", "Istasyon_modified"
  )]

  eCaps <- list(chromeOptions = list(prefs = list(
    "download.default_directory" = normalizePath(result_dir),
    "download.prompt_for_download" = FALSE,
    "download.directory_upgrade" = TRUE,
    "safebrowsing.enabled" = TRUE
  )))

 
  remote_driver <- rsDriver(browser = "chrome", port = 4445L, chromever = NULL, verbose = FALSE
                            , extraCapabilities = eCaps, phantomver = NULL)
  
  remDr <- remote_driver$client
  on.exit(try(remDr$close(), silent = TRUE), add = TRUE)
  on.exit(try(remote_driver$server$stop(), silent = TRUE), add = TRUE)
  remDr$maxWindowSize()

  remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")
  Sys.sleep(5)

  get_site_catalog <- function(timeout = 30, interval = 0.5) {
    started_at <- Sys.time()
    repeat {
      catalog_json <- tryCatch(
        remDr$executeScript(paste0(
          "if (typeof _StationDataDownload === 'undefined' || ",
          "!_StationDataDownload.Base || !_StationDataDownload.Base.Defaults) return '';",
          "var d = _StationDataDownload.Base.Defaults;",
          "return JSON.stringify({stations:d.StationIds,cities:d.CityId,groups:d.StationGroupId});"
        ))[[1]],
        error = function(e) ""
      )
      if (!is.null(catalog_json) && nzchar(catalog_json)) {
        return(jsonlite::fromJSON(catalog_json, simplifyDataFrame = TRUE))
      }
      if (difftime(Sys.time(), started_at, units = "secs") >= timeout) {
        stop("Could not load the ministry's current station catalogue")
      }
      Sys.sleep(interval)
    }
  }

  site_catalog <- get_site_catalog()
  site_stations <- site_catalog$stations
  site_stations$Station_Title <- trimws(as.character(site_stations$Station_Title))
  site_stations$Sehir <- site_catalog$cities$Name[
    match(site_stations$CityId, site_catalog$cities$Id)
  ]
  site_stations$Bolge <- site_catalog$groups$Name[
    match(site_stations$StationGroup, site_catalog$groups$Id)
  ]
  site_stations$Bolge[is.na(site_stations$Bolge)] <- "Other"
  site_stations$site_station_id <- as.character(site_stations$id)
  site_stations$canonical_key <- str_replace_all(
    site_stations$Station_Title,
    c(" " = "", "\\." = "", "/" = "_")
  )

  # Fall back to the normalized name only when it identifies one live station.
  matched_site_index <- match(location$Istasyon_original, site_stations$Station_Title)
  claimed_site_index <- unique(stats::na.omit(matched_site_index))
  for (i in which(is.na(matched_site_index))) {
    candidates <- which(
      site_stations$canonical_key == location$Istasyon_modified[i] &
        !seq_len(nrow(site_stations)) %in% claimed_site_index
    )
    if (length(candidates) == 1) {
      matched_site_index[i] <- candidates
      claimed_site_index <- c(claimed_site_index, candidates)
    }
  }

  location$Istasyon_site <- site_stations$Station_Title[matched_site_index]
  location$site_station_id <- site_stations$site_station_id[matched_site_index]
  location$file_key <- location$Istasyon_modified
  location$catalogue_status <- ifelse(
    is.na(location$Istasyon_site), "not_in_current_site", "current"
  )

  new_site_stations <- site_stations[
    !seq_len(nrow(site_stations)) %in% stats::na.omit(matched_site_index),
    c("Bolge", "Sehir", "Station_Title", "canonical_key", "site_station_id")
  ]
  names(new_site_stations)[names(new_site_stations) == "canonical_key"] <- "Istasyon_modified"
  names(new_site_stations)[names(new_site_stations) == "Station_Title"] <- "Istasyon_original"
  if (nrow(new_site_stations) > 0) {
    new_site_stations$Istasyon_site <- new_site_stations$Istasyon_original
    new_site_stations$file_key <- new_site_stations$Istasyon_modified
    conflicting_keys <- new_site_stations$file_key %in% location$file_key |
      duplicated(new_site_stations$file_key) |
      duplicated(new_site_stations$file_key, fromLast = TRUE)
    new_site_stations$file_key[conflicting_keys] <- paste0(
      new_site_stations$file_key[conflicting_keys], "__site_",
      substr(new_site_stations$site_station_id[conflicting_keys], 1, 8)
    )
    new_site_stations$catalogue_status <- "new_on_site"
    location <- rbind(location, new_site_stations[, names(location)])
  }

  catalogue_report <- data.frame(
    status = location$catalogue_status,
    location[, c("Bolge", "Sehir", "Istasyon_original", "Istasyon_modified", "file_key")],
    site_label = location$Istasyon_site,
    site_station_id = location$site_station_id,
    label_changed = !is.na(location$Istasyon_site) &
      location$Istasyon_original != location$Istasyon_site,
    checked_at = format(Sys.time(), tz = "Europe/Istanbul", usetz = TRUE),
    stringsAsFactors = FALSE
  )
  report_year <- format(parsed_start, "%Y")
  catalogue_report_file <- file.path(
    result_dir, paste0("station_catalogue_", report_year, ".csv")
  )
  write.csv(catalogue_report, catalogue_report_file, row.names = FALSE, fileEncoding = "UTF-8")

  change_report_file <- file.path(
    result_dir, paste0("station_catalogue_changes_", report_year, ".md")
  )
  new_rows <- catalogue_report[catalogue_report$status == "new_on_site", , drop = FALSE]
  retired_rows <- catalogue_report[
    catalogue_report$status == "not_in_current_site", , drop = FALSE
  ]
  renamed_rows <- catalogue_report[
    catalogue_report$status == "current" & catalogue_report$label_changed,
    , drop = FALSE
  ]
  md_lines <- c(
    paste0("# İstasyon kataloğu değişiklikleri — ", report_year),
    "",
    paste0("Kontrol zamanı: ", format(Sys.time(), tz = "Europe/Istanbul", usetz = TRUE)),
    "",
    paste0(
      "Karşılaştırma: Bakanlığın kontrol anındaki canlı kataloğu ile temizhavaR ",
      "veritabanındaki istasyon kataloğu. Bakanlık servisi eklenme/kaldırılma tarihi ",
      "vermediğinden bu liste değişikliğin hangi yıl gerçekleştiğini tek başına kanıtlamaz."
    ),
    "",
    paste0("## Canlı katalogda yeni kayıtlar (", nrow(new_rows), ")"),
    "",
    if (nrow(new_rows)) paste0(
      "- ", new_rows$site_label, " — ", new_rows$Sehir,
      " — Bakanlık ID: `", new_rows$site_station_id,
      "` — dosya anahtarı: `", new_rows$file_key, "`"
    ) else "- Yok",
    "",
    paste0("## Artık canlı katalogda bulunmayan eski kayıtlar (", nrow(retired_rows), ")"),
    "",
    if (nrow(retired_rows)) paste0(
      "- ", retired_rows$Istasyon_original, " — ", retired_rows$Sehir,
      " — eski dosya anahtarı: `", retired_rows$file_key, "`"
    ) else "- Yok",
    "",
    paste0("## Etiketi değişmiş eşleşen kayıtlar (", nrow(renamed_rows), ")"),
    "",
    if (nrow(renamed_rows)) paste0(
      "- `", renamed_rows$Istasyon_original, "` → `", renamed_rows$site_label,
      "` — Bakanlık ID: `", renamed_rows$site_station_id, "`"
    ) else "- Yok"
  )
  writeLines(md_lines, change_report_file, useBytes = TRUE)
  cat(sprintf(
    paste0(
      "Station catalogue: %d current, %d new, %d no longer listed. ",
      "Reports: %s and %s\n"
    ),
    sum(catalogue_report$status == "current"),
    sum(catalogue_report$status == "new_on_site"),
    sum(catalogue_report$status == "not_in_current_site"),
    catalogue_report_file,
    change_report_file
  ))

  if (!is.null(one_station)) {
    location <- location[
      location$Istasyon_modified == one_station | location$file_key == one_station,
    ]
    if (nrow(location) == 0) {
      stop(sprintf("Station '%s' was not found in the database or current ministry catalogue", one_station))
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
    return(invisible(TRUE))
  }

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
        regions <- remDr$findElements("css selector", ".k-list-container.k-popup li")
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
          reset_btn <- remDr$findElement(using = "css selector", ".btn.btn-block.btn-xs.btn-primary")
          reset_btn$clickElement()
          Sys.sleep(2)
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
            regions <- new_remDr$findElements("css selector", ".k-list-container.k-popup li")
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
    
    detail_exists <- xlsx_is_readable(file.path(city_dir, detail_pattern))
    summary_exists <- xlsx_is_readable(file.path(city_dir, summary_pattern))
    
    success <- detail_exists && summary_exists
    
    if (!success) {
      cat(sprintf("\nDownload verification failed for %s - %s\n", istasyon_modified, data_type))
      files <- list.files(city_dir, pattern = paste0(istasyon_modified, "_[^_]+_", start_year, "-", end_year, ".xlsx"))
      cat("Files found:", paste(files, collapse=", "), "\n")
    }
    
    return(success)
  }

  handle_station_download <- function(remDr, station_info, data_type, startdate, enddate, result_dir, max_time = 300) {
    start_time <- Sys.time()
    attempt <- 0L
    max_attempts <- 3L
    last_error <- NULL
    
    while(difftime(Sys.time(), start_time, units="secs") < max_time && attempt < max_attempts) {
      attempt <- attempt + 1L
      tryCatch({
        tryCatch({
          reset_btn <- remDr$findElement(using = "css selector", ".btn.btn-block.btn-xs.btn-primary")
          reset_btn$clickElement()
          Sys.sleep(3)
        }, error = function(e) {})
        
        missing <- download_data(
          remDr = remDr,
          bolge = station_info$Bolge,
          sehir = station_info$Sehir,
          istasyon = station_info$Istasyon_site,
          data_type = data_type,
          startdate = startdate,
          enddate = enddate,
          result_dir = result_dir,
          station_file_key = station_info$file_key,
          station_site_id = station_info$site_station_id
        )
        
        if (length(missing) == 0) {
          return(NULL)
        }
        last_error <- paste(unlist(missing), collapse = "; ")
        if (any(startsWith(unlist(missing), "NO_DATA:"))) {
          return(structure(last_error, class = c("temizhava_no_data", "character")))
        }
        cat(sprintf("Download verification failed (attempt %d/%d): %s\n",
                    attempt, max_attempts, last_error))
        if (attempt < max_attempts) {
          Sys.sleep(5)
          remDr <- ensure_session(remDr)
        }
        
      }, error = function(e) {
        last_error <<- e$message
        cat(sprintf("Download attempt %d/%d failed: %s\n", attempt, max_attempts, e$message))
        if (attempt < max_attempts) {
          Sys.sleep(5)
          remDr <<- ensure_session(remDr)
        }
      })
    }
    
    return(sprintf("Download failed for %s - %s after %d attempts: %s",
                   station_info$Istasyon_original, data_type, attempt, last_error))
  }

  all_downloads_completed <- TRUE  
  all_missing_files <- list()

  location_by_city <- split(location, location$Sehir)
  
  failed_stations <- list(
    daily = character(0),
    hourly = character(0)
  )
  no_data_stations <- list(
    daily = character(0),
    hourly = character(0)
  )
  not_listed_stations <- character(0)
  skipped_stations <- character(0)

  for (city_name in names(location_by_city)) {
    city_stations <- location_by_city[[city_name]]
    cat("\n--- Processing City:", city_name, "---\n")
    
    city_dir <- file.path(result_dir, city_name)
    if (!dir.exists(city_dir)) {
      cat("Creating directory:", city_dir, "\n")
      dir.create(city_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    city_station_modified <- as.character(city_stations$file_key)
    
    daily_downloads_needed <- batch_download_check(
      city_dir, 
      city_station_modified, 
      "daily", 
      startdate, 
      enddate,
      recheck_no_data = recheck_no_data
    )
    
    hourly_downloads_needed <- batch_download_check(
      city_dir, 
      city_station_modified, 
      "hourly", 
      startdate, 
      enddate,
      recheck_no_data = recheck_no_data
    )
    
    for (i in 1:nrow(city_stations)) {
      current_station <- city_stations[i, , drop = FALSE]
      
      cat("\n--- Processing Station:", i, "of", nrow(city_stations), "---\n")
      print(current_station)
      
      bolge <- as.character(current_station$Bolge)
      sehir <- as.character(current_station$Sehir)
      istasyon_original <- as.character(current_station$Istasyon_original)
      istasyon_modified <- as.character(current_station$Istasyon_modified)
      file_key <- as.character(current_station$file_key)
      istasyon_site <- as.character(current_station$Istasyon_site)
        
      cat("Bolge:", bolge, "\n")
      cat("Sehir:", sehir, "\n")
      cat("Istasyon:", istasyon_original, "\n")
      
      if (!(file_key %in% daily_downloads_needed) &&
          !(file_key %in% hourly_downloads_needed)) {
        skipped_stations <- c(skipped_stations, 
                             sprintf("%s (%s)", istasyon_original, sehir))
        cat("All files already exist for this station. Skipping.\n")
        next
      }

      if (is.na(istasyon_site) || !nzchar(istasyon_site)) {
        start_year <- format(as.Date(startdate, format = "%d.%m.%Y"), "%Y")
        end_year <- format(as.Date(enddate, format = "%d.%m.%Y"), "%Y")
        for (data_type in c("daily", "hourly")) {
          downloads_needed <- if (data_type == "daily") {
            daily_downloads_needed
          } else {
            hourly_downloads_needed
          }
          if (!file_key %in% downloads_needed) next

          marker <- file.path(
            city_dir,
            paste0(
              file_key, "_",
              if (data_type == "daily") "gunluk" else "saatlik",
              "_no_data_", start_year, "-", end_year, ".txt"
            )
          )
          writeLines(
            c(
              paste("Station:", istasyon_original),
              paste("Data type:", data_type),
              paste("Period:", startdate, "to", enddate),
              "Status: Station is not present in the ministry's current download catalogue.",
              paste("Checked at:", format(Sys.time(), tz = "Europe/Istanbul"))
            ),
            marker
          )
        }
        not_listed_stations <- c(
          not_listed_stations,
          sprintf("%s (%s)", istasyon_original, sehir)
        )
        cat("Station is not in the ministry's current catalogue; recorded and continuing.\n")
        next
      }

      if (!identical(istasyon_site, istasyon_original)) {
        cat("Using current ministry station label:", istasyon_site, "\n")
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
      
      if (file_key %in% daily_downloads_needed) {
        cat("Downloading daily data for:", istasyon_original, "\n")
        
        error_msg <- handle_station_download(
          remDr = remDr,
          station_info = current_station,
          data_type = "daily",
          startdate = startdate,
          enddate = enddate,
          result_dir = result_dir
        )
        
        if (inherits(error_msg, "temizhava_no_data")) {
          no_data_stations$daily <- c(no_data_stations$daily,
                                      sprintf("%s (%s)", istasyon_original, sehir))
          cat("The ministry reports no daily data for this period; recorded and continuing.\n")
        } else if (!is.null(error_msg)) {
          all_downloads_completed <- FALSE
          failed_stations$daily <- c(failed_stations$daily, 
                                   sprintf("%s (%s)", istasyon_original, sehir))
          all_missing_files <- c(all_missing_files, error_msg)
          cat("Daily download failed after timeout, skipping to next...\n")
        }
        
        if (!inherits(error_msg, "temizhava_no_data") &&
            check_download_success(city_dir, file_key, "daily", startdate, enddate)) {
          get_cached_files(city_dir, force_refresh = TRUE)
        }
      }

      if (file_key %in% hourly_downloads_needed) {
        cat("Downloading hourly data for:", istasyon_original, "\n")
        
        error_msg <- handle_station_download(
          remDr = remDr,
          station_info = current_station,
          data_type = "hourly",
          startdate = startdate,
          enddate = enddate,
          result_dir = result_dir
        )
        
        if (inherits(error_msg, "temizhava_no_data")) {
          no_data_stations$hourly <- c(no_data_stations$hourly,
                                       sprintf("%s (%s)", istasyon_original, sehir))
          cat("The ministry reports no hourly data for this period; recorded and continuing.\n")
        } else if (!is.null(error_msg)) {
          all_downloads_completed <- FALSE
          failed_stations$hourly <- c(failed_stations$hourly, 
                                    sprintf("%s (%s)", istasyon_original, sehir))
          all_missing_files <- c(all_missing_files, error_msg)
          cat("Hourly download failed after timeout, skipping to next...\n")
        }
        
        if (!inherits(error_msg, "temizhava_no_data") &&
            check_download_success(city_dir, file_key, "hourly", startdate, enddate)) {
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

  if (length(no_data_stations$daily) > 0 || length(no_data_stations$hourly) > 0) {
    cat("\n=== Ministry No-Data Report ===\n")
    if (length(no_data_stations$daily) > 0) {
      cat("Daily:\n", paste("-", no_data_stations$daily), sep = "\n")
    }
    if (length(no_data_stations$hourly) > 0) {
      cat("Hourly:\n", paste("-", no_data_stations$hourly), sep = "\n")
    }
  }

  if (length(not_listed_stations) > 0) {
    cat("\n=== Not In Current Ministry Catalogue ===\n")
    cat(paste("-", unique(not_listed_stations)), sep = "\n")
    cat("\n")
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

  cat("\nDatabase connection closed.\n")

  invisible(all_downloads_completed)
}


# Examples; sourcing this file does not start a download.
# download_temizhava_data(mode = "yearly", year = 2025)
# download_temizhava_data(mode = "yearly", year = 2024)
# download_temizhava_data(mode = "yearly", year = 2025,
#                         one_station = "İstanbul-Alibeyköy")
