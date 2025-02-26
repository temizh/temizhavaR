# This function reads data from Excel files and writes it to the database 
# while ensuring no duplicate entries are added.

# 1. Retrieve the station list from the location database.
# 2. Identify relevant files in the directory and read them.
# 3. Check if the data already exists in the database.
# 4. If the data is not present, write it to the database.
# 5. If the data is already present, skip writing it.
# 6. Return the number of rows written to the database.
# 7. If there are any missing (NA) values in the data, save them as "-".



library(DBI)
library(RSQLite)
library(temizhavaR)
library(readxl)
library(stringr)
library(dplyr)

read_and_write_data <- function(delete_previous = FALSE) {
  db <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
  
  # if (delete_previous) {
  #   dbExecute(db, "DELETE FROM daily_detail")
  #   dbExecute(db, "DELETE FROM hourly_detail")
  #   cat("Previous data cleared from tables\n")
  # }

  location <- dbReadTable(db, "location")
  
  daily_table  <- "daily_detail"
  hourly_table <- "hourly_detail"
  
  data_dir <- "./TemizHava_raw_data"
  files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

  
  rows_written <- 0
  
  for (file in files) {
    # If station Konya-Selçuklu-Belediye skip it because no data is available
    if (str_detect(file, "Konya-Selçuklu-Belediye")) {
      cat("Skipping file:", file, "as no data is available\n")
      next
    }
    if (str_detect(file, "gunluk_detay")) {
      target_table <- daily_table
      cat("Reading daily data from file:", file, "\n")
    } else if (str_detect(file, "saatlik_detay")) {
      target_table <- hourly_table
      cat("Reading hourly data from file:", file, "\n")
    } else {
      cat("File does not contain gunluk_detay or saatlik_detay:", file, "\n")
      next
    }
    
    raw_data <- tryCatch(
      read_excel(file, col_names = FALSE),
      error = function(e) {
        cat("Error reading file:", file, "\n", e$message, "\n")
        return(NULL)
      }
    )
    if (is.null(raw_data)) next

    
      if (nrow(raw_data) < 2) {
      cat("Not enough rows in file for headers:", file, "\n")
      next
    }
    header1 <- as.character(unlist(raw_data[1, ]))
    header2 <- as.character(unlist(raw_data[2, ]))

    combined_header <- ifelse(is.na(header2) | header2 == "", header1, header2)
    combined_header <- trimws(combined_header)
    combined_header <- sub(" \\(.*\\)$", "", combined_header)
    # Remove spaces in combined_header items 
    combined_header <- gsub(" ", "", combined_header)
    print(paste("Combined header:", combined_header))
    
    names(raw_data) <- combined_header
    data <- raw_data[-c(1, 2), ]
    
    # Remove completely empty rows
    data <- data[rowSums(is.na(data)) != ncol(data), ]
    
    base_name <- tools::file_path_sans_ext(basename(file))
    # ./TemizHava_raw_data/Adana/Adana-Valilik_gunluk_ozet_2014-2024.xlsx 
    # We want Adana-Valilik
    station_extracted <- str_extract(base_name, ".*(?=_gunluk|_saatlik)")
    if (is.na(station_extracted)) {
      cat("Could not extract station name from file:", file, "\n")
      next
    }

    # Get Istasyonlar and Id from location table, in hourly and daily tables, the column name is different
    # Istasyon and location_id

    location <- location %>%
       select(Istasyonlar_modified, Id)
     
    

    
    if (!any(grepl(station_extracted, location$Istasyonlar_modified, fixed = TRUE))) {
      cat("Station not found in location table:", station_extracted, "\n")
      next
    }
    
   
    
    dup_check <- dbGetQuery(db, paste0(
      "SELECT * FROM ", target_table, " WHERE Istasyon_modified = ? AND Tarih = ?"
    ), params = list(station_extracted, data$Tarih[1]))

    if (nrow(dup_check) > 0) {
      cat("Data already exists in table", target_table, "for station:", station_extracted, "\n")
      next
    }

    
    data <- data %>% 
      mutate(across(-Tarih, ~ replace(., is.na(.), "-")))

    data$Istasyon_modified <- str_replace_all(station_extracted, c(" " = "", "\\." = "", "/" = "_"))


    
    loc_idx <- which(grepl(station_extracted, location$Istasyonlar_modified, fixed = TRUE))
    if (length(loc_idx) == 0) {
      cat("Station", station_extracted, "not found in location table.\n")
      next
    }
    location_id <- location$Id[loc_idx[1]]
    location_name <- location$Istasyonlar[loc_idx[1]]
   
    # data$Istasyon    <- station_extracted
    data$location_id <- location_id
    data$Istasyon <- location_name
    
    # Clean data: replace NA with "-" and remove any remaining empty rows
    data <- data %>% 
      mutate(across(everything(), ~ifelse(is.na(.) | . == "", "-", .))) %>%
      filter(Tarih != "-") 

    expected_cols <- c("Istasyon", "location_id", "Tarih", "PM10", "PM2.5", "SO2", "CO", "NO2", "NOX", "NO", "O3", "Istasyon_modified")
    for (col in expected_cols) {
      if (!col %in% names(data)) {
        data[[col]] <- "-"
      }
    }


    data <- data[, expected_cols]

    dbWriteTable(db, target_table, data, row.names = FALSE, append = TRUE)
    rows_written <- rows_written + nrow(data)
    
    cat("Data written to", target_table, "for station:", station_extracted, "\n")
  }
  
  dbDisconnect(db)
  
  return(rows_written)
}

rows_written <- read_and_write_data()
cat("Total rows written:", rows_written, "\n")
