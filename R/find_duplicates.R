library(data.table)
library(readxl)
library(progress)
library(stringr)
library(openxlsx)

find_station_duplicates <- function(pattern = "\\.xlsx$", data_dir) {
  
    if(missing(data_dir)) {
        stop("data_dir argument is required!")
    }
    logs_dir <- file.path(data_dir, "logs")
    if(!dir.exists(logs_dir)) {
        dir.create(logs_dir)
    }
    

  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  excel_output <- file.path(logs_dir, sprintf("duplicate_analysis_%s.xlsx", timestamp))
  
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  total_files <- length(files)
  
  if(total_files == 0) {
    stop("No Excel files found!")
  }
  
  cat(sprintf("Found %d files to analyze\n", total_files))
  
  results <- list()
  summary_data <- data.frame(
    Station = character(),
    Filename = character(),
    Total_Rows = integer(),
    Unique_Rows = integer(),
    Duplicate_Rows = integer(),
    Dates_With_Duplicates = integer(),
    stringsAsFactors = FALSE
  )
  
  duplicate_details <- data.frame(
    Station = character(),
    Filename = character(),
    Duplicate_Type = character(),
    Tarih = character(),
    PM10 = numeric(),
    PM25 = numeric(),
    SO2 = numeric(),
    NO2 = numeric(),
    NOX = numeric(),
    NO = numeric(),
    O3 = numeric(),
    CO = numeric(),
    stringsAsFactors = FALSE
  )
  
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent | File: :current/:total | ETA: :eta",
    total = total_files,
    clear = FALSE,
    width = 80
  )
  
  for(file_idx in seq_along(files)) {
    file <- files[file_idx]
    filename <- basename(file)
    
    station_name <- str_extract(filename, ".*(?=_gunluk|_saatlik)")
    if(is.na(station_name)) next
    
    if(!str_detect(filename, "detay")) {
      cat(sprintf("Skipping non-detail file: %s\n", filename))
      next
    }
    
    tryCatch({
      headers <- read_excel(file, n_max = 2, col_names = FALSE)
      
      df <- as.data.table(read_excel(file, 
                                   skip = 2, 
                                   col_names = FALSE,
                                   col_types = "text"))  
      
      n_cols <- ncol(df)
      
      header1 <- as.character(headers[1,])[1:n_cols]
      header2 <- as.character(headers[2,])[1:n_cols]
      
      combined_headers <- character(n_cols)
      for(i in seq_len(n_cols)) {
        h1 <- if(!is.na(header1[i])) trimws(header1[i]) else paste0("Col", i)
        h2 <- if(!is.na(header2[i]) && header2[i] != "") trimws(header2[i]) else ""
        
        if(i == 1 && tolower(h1) == "tarih") {
          combined_headers[i] <- "Tarih"
          next
        }
        
        combined_headers[i] <- if(h2 == "") {
          gsub("\\s*\\([^)]+\\)", "", h1) 
        } else {
          paste(gsub("\\s*\\([^)]+\\)", "", h1),
                gsub("\\s*\\([^)]+\\)", "", h2), 
                sep = "_")
        }
        combined_headers[i] <- gsub("\\s+", "", combined_headers[i]) 
      }
      
      setnames(df, combined_headers)
      
      if("Tarih" %in% combined_headers) {
        tarih_col <- "Tarih"
      } else {
        tarih_col <- names(df)[grep("(?i)tarih|date", names(df))]
        if(length(tarih_col) == 0) {
          warning(sprintf("No date column found in %s", filename))
          next
        }
        tarih_col <- tarih_col[1]
        setnames(df, tarih_col, "Tarih")
        tarih_col <- "Tarih"
      }
      
      cat("\nDate column name:", tarih_col)
      cat("\nFirst few raw date values:\n")
      print(head(df[[tarih_col]]))
      
      df[, (tarih_col) := {
        dates <- get(tarih_col)
        clean_dates <- trimws(dates)
        clean_dates[clean_dates %in% c("", "-", "NA", "NULL")] <- NA
        
        parsed_dates <- try({
          excel_nums <- as.numeric(clean_dates)
          
          excel_base <- as.POSIXct("1899-12-30 00:00:00", tz = "UTC")
          
          seconds <- excel_nums * 86400
          
          excel_base + seconds
        }, silent = TRUE)
        
        if(inherits(parsed_dates, "try-error") || all(is.na(parsed_dates))) {
          parsed_dates <- as.POSIXct(clean_dates, format = "%d.%m.%Y %H:%M:%S", tz = "UTC")
        }
        
        cat("\nFirst few parsed dates:\n")
        print(head(parsed_dates))
        
        if(all(is.na(parsed_dates))) {
          warning(sprintf("Could not parse any dates in %s", filename))
        }
        
        parsed_dates
      }]
      
      if(all(is.na(df[[tarih_col]]))) {
        warning(sprintf("All dates are NA in %s - skipping file", filename))
        next
      }

      numeric_cols <- setdiff(names(df), tarih_col)
      df[, (numeric_cols) := lapply(.SD, function(x) {
        x[x == "-"] <- NA
        as.numeric(gsub(",", ".", x))
      }), .SDcols = numeric_cols]

      total_rows <- nrow(df)
      unique_rows <- uniqueN(df)
      
      date_duplicates <- df[, .N, by = get(tarih_col)][N > 1]
      
      duplicate_dates <- NULL
      if(nrow(date_duplicates) > 0) {
        duplicate_dates <- df[df[, .I[.N > 1], by = get(tarih_col)]$V1]
        setorderv(duplicate_dates, tarih_col)
      }
      
      complete_duplicates <- df[df[, .I[.N > 1], by = names(df)]$V1]
      
      if(total_rows > unique_rows || nrow(date_duplicates) > 0) {
        col_mapping <- list()
        for(col in names(df)) {
          if(grepl("PM10", col, ignore.case = TRUE)) col_mapping[["PM10"]] <- col
          if(grepl("PM25|PM2.5", col, ignore.case = TRUE)) col_mapping[["PM25"]] <- col
          if(grepl("SO2", col, ignore.case = TRUE)) col_mapping[["SO2"]] <- col
          if(grepl("NO2", col, ignore.case = TRUE)) col_mapping[["NO2"]] <- col
          if(grepl("^NOX", col, ignore.case = TRUE)) col_mapping[["NOX"]] <- col
          if(grepl("^NO$|^NO_", col, ignore.case = TRUE)) col_mapping[["NO"]] <- col
          if(grepl("O3", col, ignore.case = TRUE)) col_mapping[["O3"]] <- col
          if(grepl("CO", col, ignore.case = TRUE)) col_mapping[["CO"]] <- col
        }

        results[[station_name]] <- list(
          file = filename,
          total_rows = total_rows,
          unique_rows = unique_rows,
          duplicate_rows = total_rows - unique_rows,
          date_duplicates = nrow(date_duplicates),
          duplicate_dates = duplicate_dates,
          complete_duplicates = complete_duplicates
        )
        
        summary_data <- rbind(summary_data, data.frame(
          Station = station_name,
          Filename = filename,
          Total_Rows = total_rows,
          Unique_Rows = unique_rows,
          Duplicate_Rows = total_rows - unique_rows,
          Dates_With_Duplicates = nrow(date_duplicates)
        ))
        
        if(nrow(complete_duplicates) > 0) {
          
          measurements <- data.frame(
            Station = station_name,
            Filename = filename,
            Duplicate_Type = "Complete Row",
            Tarih = format(complete_duplicates$Tarih)
          )
          
          for(std_col in c("PM10", "PM25", "SO2", "NO2", "NOX", "NO", "O3", "CO")) {
            measurements[[std_col]] <- if(!is.null(col_mapping[[std_col]])) {
              complete_duplicates[[col_mapping[[std_col]]]]
            } else {
              rep(NA_real_, nrow(complete_duplicates))
            }
          }
          
          duplicate_details <- rbind(duplicate_details, measurements)
        }
        
        if(!is.null(duplicate_dates)) {
          measurements <- data.frame(
            Station = station_name,
            Filename = filename,
            Duplicate_Type = "Date Only",
            Tarih = format(duplicate_dates$Tarih)
          )
          
          for(std_col in c("PM10", "PM25", "SO2", "NO2", "NOX", "NO", "O3", "CO")) {
            measurements[[std_col]] <- if(!is.null(col_mapping[[std_col]])) {
              duplicate_dates[[col_mapping[[std_col]]]]
            } else {
              rep(NA_real_, nrow(duplicate_dates))
            }
          }
          
          duplicate_details <- rbind(duplicate_details, measurements)
        }
        
        cat(sprintf("\n⚠️ Found duplicates in %s:\n", filename))
        cat(sprintf("  - Total duplicated rows: %d\n", total_rows - unique_rows))
        cat(sprintf("  - Dates with duplicates: %d\n", nrow(date_duplicates)))
        
        if(nrow(complete_duplicates) > 0) {
          cat("\nComplete row duplicates:\n")
          print(complete_duplicates)
        }
        
        if(!is.null(duplicate_dates)) {
          cat("\nDate duplicates:\n")
          print(duplicate_dates)
        }
      }
      
    }, error = function(e) {
      cat(sprintf("\n❌ Error processing %s: %s\n", filename, conditionMessage(e)))
    })
    
    pb$tick()
  }
  
  cat("\n📊 Summary Report:\n")
  cat("================\n")
  
  if(length(results) == 0) {
    cat("✓ No duplicates found in any station data!\n")
  } else {
    cat(sprintf("Found duplicates in %d stations:\n\n", length(results)))
    for(station in names(results)) {
      r <- results[[station]]
      cat(sprintf("\n=== Station: %s ===\n", station))
      cat(sprintf("File: %s\n", r$file))
      cat(sprintf("Total rows: %d\n", r$total_rows))
      cat(sprintf("Duplicate rows: %d\n", r$duplicate_rows))
      cat(sprintf("Dates with duplicates: %d\n", r$date_duplicates))
      
      if(nrow(r$complete_duplicates) > 0) {
        cat("\nComplete row duplicates:\n")
        print(r$complete_duplicates)
      }
      
      if(!is.null(r$duplicate_dates)) {
        cat("\nDate duplicates:\n")
        print(r$duplicate_dates)
      }
      
      cat("\n----------------------------------------\n")
    }
  }
  
  if(nrow(summary_data) > 0) {
    wb <- createWorkbook()
    
    addWorksheet(wb, "Summary")
    writeData(wb, "Summary", summary_data)
    
    if(nrow(duplicate_details) > 0) {
      addWorksheet(wb, "Duplicate Details")
      writeData(wb, "Duplicate Details", duplicate_details)
      
      number_style <- createStyle(numFmt = "0.00")
      measurement_cols <- c("PM10", "PM25", "SO2", "NO2", "NOX", "NO", "O3", "CO")
      for(col in which(names(duplicate_details) %in% measurement_cols)) {
        addStyle(wb, "Duplicate Details", number_style, 
                rows = 2:(nrow(duplicate_details) + 1), 
                cols = col, 
                gridExpand = TRUE)
      }
      
      setColWidths(wb, "Duplicate Details", 
                  cols = 1:ncol(duplicate_details),
                  widths = "auto")
    }
    
    style_header <- createStyle(
      textDecoration = "bold",
      border = "bottom",
      fgFill = "#E0E0E0"
    )
    
    addStyle(wb, "Summary", style_header, rows = 1, cols = 1:ncol(summary_data))
    if(nrow(duplicate_details) > 0) {
      addStyle(wb, "Duplicate Details", style_header, 
              rows = 1, cols = 1:ncol(duplicate_details))
    }
    
    saveWorkbook(wb, excel_output, overwrite = TRUE)
    cat(sprintf("\n✓ Results saved to: %s\n", excel_output))
  } else {
    cat("\n✓ No duplicates found in any files\n")
  }
  
  invisible(results)
}

# find_station_duplicates()
#  specific pattern:
# find_station_duplicates(pattern = "saatlik_detay.*\\.xlsx$")
