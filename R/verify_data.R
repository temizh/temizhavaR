library(DBI)
library(dplyr)
library(temizhavaR)

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

#' Check data for a specific station and date range
#' @param station_name Station name
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param start_hour Hour (1-12 for AMPM format, 0-23 for 24h format)
#' @param end_hour Hour (1-12 for AMPM format, 0-23 for 24h format)
#' @param start_meridiem "AM" or "PM" (only for AMPM format)
#' @param end_meridiem "AM" or "PM" (only for AMPM format)
#' @param time_format "24h" or "AMPM"
#' @param table_type "hourly" or "daily"
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr filter
#' @export
check_station_data <- function(station_name, start_date, end_date, 
                             start_hour = 0, end_hour = 23, 
                             start_meridiem = NULL, end_meridiem = NULL,
                             time_format = "24h", table_type = "hourly") {
  
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
  on.exit(disconnect_postgres(db))
  
  table_name <- if(table_type == "hourly") "hourly_detail" else "daily_detail"
  
  start_timestamp <- format(as.POSIXct(paste(start_date, sprintf("%02d:00:00", start_hour)), tz = "UTC"))
  end_timestamp <- format(as.POSIXct(paste(end_date, sprintf("%02d:59:59", end_hour)), tz = "UTC"))
  
  query <- sprintf(
    'SELECT *
     FROM %s
     WHERE "Istasyon_modified" = $1
     AND "Tarih" BETWEEN $2::timestamptz AND $3::timestamptz
     ORDER BY "Tarih"',
    table_name
  )
  
  data <- dbGetQuery(
    db,
    query,
    params = list(station_name, start_timestamp, end_timestamp)
  )
  
  if (nrow(data) == 0) {
    cat("No data found for the specified period\n")
    return(NULL)
  }
  
  cat(sprintf("\nData summary for %s (%s to %s):\n",
              station_name, start_date, end_date))
  cat(sprintf("Total records: %d\n", nrow(data)))
  cat(sprintf("Date range: %s to %s\n",
              min(data$Tarih), max(data$Tarih)))
  
  if (table_type == "hourly") {
    total_days <- as.numeric(difftime(as.Date(end_date), as.Date(start_date), units = "days"))
    if (total_days == 0) {
      expected_hours <- end_hour - start_hour + 1
    } else {
      expected_hours <- (24 - start_hour) + 
        (total_days - 1) * 24 + 
        (end_hour + 1) 
    }
    
    data$Tarih <- as.POSIXct(data$Tarih, tz = "UTC")
    actual_timestamps <- sort(data$Tarih)
    expected_timestamps <- seq(
      from = as.POSIXct(start_timestamp, tz = "UTC"),
      to = as.POSIXct(end_timestamp, tz = "UTC"),
      by = "hour"
    )
    
    missing_times <- expected_timestamps[!expected_timestamps %in% actual_timestamps]
    
    if (length(missing_times) > 0) {
      cat("\nWARNING: Found gaps in data\n")
      cat(sprintf("Expected %.0f records, found %.0f\n",
                  expected_hours, nrow(data)))
      
      missing_pct <- length(missing_times) / expected_hours * 100
      cat(sprintf("Missing data: %.1f%%\n", missing_pct))
      
      if (length(missing_times) < 10) {
        cat("\nMissing timestamps:\n")
        print(format(missing_times, "%Y-%m-%d %H:%M:%S"))
      }
    }
  }
  
  return(data)
}

#' Summarize data quality for a station
#' @param data Data frame returned by check_station_data
#' @export
summarize_data_quality <- function(data) {
  if (is.null(data)) return(NULL)
  
  numeric_cols <- c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3")
  data[numeric_cols] <- lapply(data[numeric_cols], function(x) {
    if (is.character(x)) {
      x[x %in% c("", "-", "NULL", "NA", "NaN", "*", "N/A")] <- NA
      suppressWarnings(as.numeric(x))
    } else {
      as.numeric(x)
    }
  })
  
  cat("\nColumn types after conversion:\n")
  print(sapply(data[numeric_cols], class))
  
  missing_summary <- sapply(data[numeric_cols],
                          function(x) sum(is.na(x)) / length(x) * 100)
  
  cat("\nMissing values percentage by parameter:\n")
  print(round(missing_summary, 2))
  
  cat("\nValue ranges by parameter:\n")
  value_summary <- sapply(data[numeric_cols], function(x) {
    tryCatch({
      if (all(is.na(x))) {
        "All NA"
      } else {
        vals <- x[!is.na(x)]
        if (length(vals) > 0) {
          sprintf("Range: %.2f - %.2f (Mean: %.2f, n=%d)", 
                 min(vals), max(vals), mean(vals), length(vals))
        } else {
          "No valid values"
        }
      }
    }, error = function(e) "Error calculating statistics")
  })
  print(value_summary)
  
  cat("\nFirst few records:\n")
  records <- head(data[c("Tarih", numeric_cols)])
  print(records, digits = 2)
  
  invisible(list(
    missing_summary = missing_summary,
    value_summary = value_summary,
    record_count = nrow(data),
    date_range = range(data$Tarih)
  ))
}

data <- check_station_data(
  station_name = "Adana-Seyhan",
  start_date = "2021-02-20",
  end_date = "2021-02-21",
  start_hour = 4,
  start_meridiem = "AM",
  end_hour = 1,
  end_meridiem = "PM",
  time_format = "AMPM",
  table_type = "hourly"
)

summarize_data_quality(data)
