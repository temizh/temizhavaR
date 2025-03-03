library(DBI)
library(RSQLite)
library(temizhavaR)
library(readxl)
library(stringr)
library(dplyr)
library(dbplyr)


# checkleri çalıştır

#' Convert 12-hour time to 24-hour format
#' @param hour Numeric hour (1-12)
#' @param meridiem "AM" or "PM"
#' @return Numeric hour in 24-hour format (0-23)
convert_to_24h <- function(hour, meridiem) {
  hour <- as.numeric(hour)
  if (meridiem == "PM" && hour < 12) hour <- hour + 12
  if (meridiem == "AM" && hour == 12) hour <- 0
  return(hour)
}

read_and_write_data <- function(delete_previous = FALSE, pattern, overwrite_data_dict = NULL, 
                               start_hour = 0, end_hour = 23,
                               start_meridiem = NULL, end_meridiem = NULL,
                               time_format = "24h") {
  
  if (time_format == "AMPM") {
    if (is.null(start_meridiem) || is.null(end_meridiem)) {
      stop("start_meridiem and end_meridiem required for AMPM format")
    }
    if (!start_meridiem %in% c("AM", "PM") || !end_meridiem %in% c("AM", "PM")) {
      stop("meridiem must be 'AM' or 'PM'")
    }
    if (!between(start_hour, 1, 12) || !between(end_hour, 1, 12)) {
      stop("Hours must be between 1 and 12 for AMPM format")
    }
    
    start_hour <- convert_to_24h(start_hour, start_meridiem)
    end_hour <- convert_to_24h(end_hour, end_meridiem)
  } else {
    if (!between(start_hour, 0, 23) || !between(end_hour, 0, 23)) {
      stop("Hours must be between 0 and 23 for 24h format")
    }
  }
  
  db <- create_postgres_conn()
  
  validate_date <- function(date_str) {
    tryCatch({
      date <- as.Date(date_str)
      if (is.na(date)) return(FALSE)
      format(date, "%Y-%m-%d") == date_str
    }, error = function(e) FALSE)
  }
  
  if (!is.null(overwrite_data_dict)) {
    for (station in names(overwrite_data_dict)) {
      date_range <- overwrite_data_dict[[station]]
      if (!all(sapply(date_range, validate_date))) {
        stop(sprintf("Invalid date(s) in range for station %s: %s to %s", 
                    station, date_range[1], date_range[2]))
      }
    }
  }

  if (delete_previous) {
    dbExecute(db, "DELETE FROM daily_detail")
    dbExecute(db, "DELETE FROM hourly_detail")
    cat("Previous data cleared from tables\n")
  }

  if (!is.null(overwrite_data_dict)) {
    for (station in names(overwrite_data_dict)) {
      date_range <- overwrite_data_dict[[station]]
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      
      start_timestamp <- format(as.POSIXct(
        paste(date_range[1], sprintf("%02d:00:00", start_hour)), 
        tz = "UTC"
      ))
      
      end_hour_adjusted <- end_hour
      end_date_adjusted <- end_date
      if (time_format == "AMPM" && end_meridiem == "AM" && end_hour < 12) {
        end_date_adjusted <- end_date + 1
      }
      
      end_timestamp <- format(as.POSIXct(
        paste(end_date_adjusted, sprintf("%02d:59:59", end_hour_adjusted)), 
        tz = "UTC"
      ))
      
      cat(sprintf("Using time range: %s to %s\n", start_timestamp, end_timestamp))
      
      tables_to_update <- if (!is.null(pattern)) {
        if (str_detect(pattern, "gunluk_detay|_daily")) {
          c("daily_detail")
        } else if (str_detect(pattern, "saatlik_detay|_hourly")) {
          c("hourly_detail")
        } else {
          c("daily_detail", "hourly_detail")
        }
      } else {
        c("daily_detail", "hourly_detail")
      }
      
      cat(sprintf("Will update tables: %s\n", paste(tables_to_update, collapse = ", ")))
      
      for (table in tables_to_update) {
        delete_query <- sprintf(
          'DELETE FROM %s WHERE "%s"."Istasyon_modified" = \'%s\' AND 
           COALESCE("%s"."Tarih", \'1900-01-01\'::timestamptz) 
           BETWEEN \'%s\'::timestamptz AND \'%s\'::timestamptz',
          table, table,
          station, table,
          start_timestamp, end_timestamp
        )
        
        tryCatch({
          dbExecute(db, delete_query)
          cat(sprintf("Cleared data from %s for station %s between %s and %s\n", 
                     table, station, start_timestamp, end_timestamp))
        }, error = function(e) {
          cat(sprintf("Error clearing data from %s: %s\n", table, conditionMessage(e)))
        })
      }
    }
  }

  col_mapping <- c(
    "PM2.5" = "PM25"
  )

  location_tbl <- tbl(db, "location")
  daily_tbl <- tbl(db, "daily_detail")
  hourly_tbl <- tbl(db, "hourly_detail")
  
  data_dir <-  getOption("temizhavaR.base_dir")





  if (is.null(pattern)){
    pattern = "\\.xlsx$"
  }
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  
  rows_written <- 0
  batch_size <- 5000  
  
  for (file in files) {
    if (str_detect(file, "Konya-Selçuklu-Belediye")) {
      cat("Skipping file:", file, "as no data is available\n")
      next
    }
    
    target_table <- case_when(
      str_detect(file, "gunluk_detay|_daily") ~ "daily_detail",
      str_detect(file, "saatlik_detay|_hourly") ~ "hourly_detail",
      TRUE ~ NA_character_
    )
    
    if (is.na(target_table)) {
      cat("Skipping file:", file, "\n")
      next
    }
    
    station_extracted <- str_extract(basename(file), ".*(?=_gunluk|_saatlik)")
    if (is.na(station_extracted)) next
    
    raw_data <- tryCatch({
      df <- read_excel(file, col_names = FALSE) %>%
        as_tibble()
      
      header1 <- as.character(unlist(df[1, ]))
      header2 <- as.character(unlist(df[2, ]))
      combined_header <- ifelse(is.na(header2) | header2 == "", header1, header2)
      combined_header <- trimws(combined_header) %>%
        sub(" \\(.*\\)$", "", .) %>%
        gsub(" ", "", .)
      
      combined_header[combined_header %in% names(col_mapping)] <- 
        col_mapping[combined_header[combined_header %in% names(col_mapping)]]
      
      names(df) <- combined_header
      df <- df[-c(1, 2), ]
      
      df <- df %>%
        mutate(
          Tarih = case_when(
            grepl("^[0-9.]+$", Tarih) ~ as.POSIXct("1900-01-01", tz="UTC") + 
              (as.numeric(Tarih) - 2) * 86400,
            TRUE ~ as.POSIXct(strptime(Tarih, "%d.%m.%Y %H:%M:%S"), tz="UTC")
          )
        )
      
      measurement_cols <- c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3")
      df <- df %>%
        mutate(across(all_of(intersect(names(.), measurement_cols)), 
                     ~ifelse(. %in% c("", "-", "NULL", "NA", "NaN", "*", "N/A") | 
                            is.na(.) | suppressWarnings(as.numeric(.)) < 0, 
                            NA_real_,
                            suppressWarnings(as.numeric(.)))))
      df
    }, error = function(e) {
      cat("Error processing file:", file, "\n", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(raw_data)) next
    
    target_ref <- if(target_table == "daily_detail") daily_tbl else hourly_tbl
    min_date <- format(min(raw_data$Tarih), "%Y-%m-%d %H:%M:%S")
    max_date <- format(max(raw_data$Tarih), "%Y-%m-%d %H:%M:%S")
    
    if (!is.null(overwrite_data_dict) && station_extracted %in% names(overwrite_data_dict)) {
      date_range <- overwrite_data_dict[[station_extracted]]
      
      overwrite_start <- as.POSIXct(
        paste(date_range[1], sprintf("%02d:00:00", start_hour)), 
        format="%Y-%m-%d %H:%M:%S", 
        tz="UTC"
      )
      
      end_date_adjusted <- as.Date(date_range[2])
      if (time_format == "AMPM" && end_meridiem == "AM" && end_hour < 12) {
        end_date_adjusted <- end_date_adjusted + 1
      }
      
      overwrite_end <- as.POSIXct(
        paste(end_date_adjusted, sprintf("%02d:59:59", end_hour)), 
        format="%Y-%m-%d %H:%M:%S", 
        tz="UTC"
      )
      
      cat(sprintf("Filtering data between %s and %s\n", 
                 format(overwrite_start, "%Y-%m-%d %H:%M:%S"),
                 format(overwrite_end, "%Y-%m-%d %H:%M:%S")))
      
      raw_data <- raw_data %>%
        filter(
          Tarih >= overwrite_start,
          Tarih <= overwrite_end
        )
      
      if (nrow(raw_data) == 0) {
        cat(sprintf("No data found in specified date range (%s to %s) for station: %s\n",
                   format(overwrite_start, "%Y-%m-%d %H:%M:%S"), 
                   format(overwrite_end, "%Y-%m-%d %H:%M:%S"),
                   station_extracted))
        next
      }
      
      min_date <- format(min(raw_data$Tarih), "%Y-%m-%d %H:%M:%S")
      max_date <- format(max(raw_data$Tarih), "%Y-%m-%d %H:%M:%S")
      cat(sprintf("Found %d records between %s and %s\n", 
                 nrow(raw_data), min_date, max_date))
    }
    
    existing_count <- target_ref %>%
      filter(
        Istasyon_modified == station_extracted,
        Tarih >= sql(paste0("'", min_date, "'::timestamptz")),
        Tarih <= sql(paste0("'", max_date, "'::timestamptz"))
      ) %>%
      count() %>%
      collect() %>%
      pull(n)
    
    should_process <- TRUE
    if (existing_count > 0) {
      if (is.null(overwrite_data_dict)) {
        gaps_query <- sprintf(
          'SELECT COUNT(*) as gap_count FROM 
           (SELECT Tarih FROM generate_series(
             \'%s\'::timestamptz, 
             \'%s\'::timestamptz, 
             INTERVAL \'1 day\'
           ) AS Tarih) as dates 
           LEFT JOIN %s ON 
           date_trunc(\'day\', dates.Tarih) = date_trunc(\'day\', %s."Tarih") AND 
           %s."Istasyon_modified" = \'%s\'
           WHERE %s."Tarih" IS NULL',
          min_date, max_date,
          target_table, target_table, target_table,
          station_extracted, target_table
        )
        
        gaps_exist <- dbGetQuery(db, gaps_query)$gap_count > 0
        
        if (gaps_exist) {
          cat("Found gaps in existing data for station:", station_extracted, ". Processing file to fill gaps.\n")
          should_process <- TRUE
        } else {
          cat("Complete data already exists for station:", station_extracted, "\n")
          should_process <- FALSE
        }
      } else {
        if (station_extracted %in% names(overwrite_data_dict)) {
          date_range <- overwrite_data_dict[[station_extracted]]
          
          file_start_date <- as.Date(min_date)
          file_end_date <- as.Date(max_date)
          overwrite_start_date <- as.Date(date_range[1])
          overwrite_end_date <- as.Date(date_range[2])
          
          if (file_start_date <= overwrite_end_date && file_end_date >= overwrite_start_date) {
            should_process <- TRUE
            cat(sprintf("Processing file for overwrite: %s (File range: %s to %s, Overwrite range: %s to %s)\n",
                       station_extracted, file_start_date, file_end_date, 
                       overwrite_start_date, overwrite_end_date))
          } else {
            should_process <- FALSE
            cat(sprintf("File date range (%s to %s) doesn't match overwrite range (%s to %s) for station: %s\n",
                       file_start_date, file_end_date, 
                       overwrite_start_date, overwrite_end_date,
                       station_extracted))
          }
        } else {
          should_process <- FALSE
          cat("Station not in overwrite list:", station_extracted, "\n")
        }
      }
    }
    
    if (!should_process) next

    location_match <- location_tbl %>%
      filter(Istasyonlar_modified == station_extracted) %>%
      collect()
    
    if (nrow(location_match) == 0) {
      cat("No matching location found for station:", station_extracted, "\n")
      next
    }
    
    raw_data <- raw_data %>%
      mutate(
        Istasyon_modified = station_extracted,
        Istasyon = location_match$Istasyonlar[1],
        location_id = location_match$Id[1]
       
      )
    
    expected_cols <- c("Istasyon", "location_id", "Tarih", "PM10", "PM25", "SO2", 
                      "CO", "NO2", "NOX", "NO", "O3", "Istasyon_modified")
    
    for (col in setdiff(expected_cols, names(raw_data))) {
      raw_data[[col]] <- NA
    }
    
    processed_data <- raw_data %>%
      mutate(
        across(c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3"), 
               ~case_when(
                 . %in% c("", "-", "NULL", "NA", "NaN") ~ NA_character_,
                 as.numeric(.) < 0 ~ NA_character_,  
                 TRUE ~ as.character(.)
               )),
        across(c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3"),
               ~as.numeric(.)),
        Tarih = format(as.POSIXct(Tarih, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
      ) %>%
      select(all_of(expected_cols))
    
    cat("\nColumn types in processed data:\n")
    print(sapply(processed_data, class))
    
    has_data <- sapply(processed_data[c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3")],
                      function(x) any(!is.na(x)))
    if (any(has_data)) {
      cat("\nFound non-NA values in columns:", 
          paste(names(has_data)[has_data], collapse=", "), "\n")
    }
    
    if (nrow(processed_data) > 0 && !all(is.na(processed_data$Tarih))) {
      tryCatch({
        copy_to(db, processed_data, target_table, temporary = FALSE, append = TRUE)
        rows_written <- rows_written + nrow(processed_data)
        cat("Processed file:", basename(file), "\n")
      }, error = function(e) {
        cat("Error inserting data for file:", basename(file), "\n")
        cat("Error message:", conditionMessage(e), "\n")
      })
    } else {
      cat("Skipping file due to invalid data:", basename(file), "\n")
    }
  }
  
  tryCatch({
    dbExecute(db, 'ALTER TABLE daily_detail ALTER COLUMN "Tarih" TYPE TIMESTAMPTZ USING "Tarih"::timestamptz')
    dbExecute(db, 'ALTER TABLE hourly_detail ALTER COLUMN "Tarih" TYPE TIMESTAMPTZ USING "Tarih"::timestamptz')
    cat("Successfully updated timestamp columns\n")
  }, error = function(e) {
    cat("Error updating timestamp columns:", conditionMessage(e), "\n")
  })

  disconnect_postgres(db)
  return(rows_written)
}

# overwrite_dict <- list(
#   "Adana-Seyhan" = c("2023-01-01", "2023-12-31")
# )
# rows_written <- read_and_write_data(pattern = "\\.xlsx$", overwrite_data_dict = overwrite_dict)

# rows_written <- read_and_write_data()
# rows_written <- read_and_write_data(pattern = "Adana-Seyhan_saatlik_detay_2014-2024.xlsx")
rows_written <- read_and_write_data(pattern = "\\.xlsx$", delete_previous = TRUE)



# overwrite_dict <- list(
#   "Adana-Seyhan" = c("2021-02-20", "2021-02-21")
# )
# rows_written <- read_and_write_data(
#   pattern = "Adana-Seyhan_saatlik_detay_2014-2024.xlsx",
#   overwrite_data_dict = overwrite_dict,
#   start_hour = 4,
#   start_meridiem = "AM",
#   end_hour = 1,
#   end_meridiem = "PM",
#   time_format = "AMPM"
# )


cat("Total rows written:", rows_written, "\n")
