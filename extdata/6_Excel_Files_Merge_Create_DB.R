library(DBI)
library(RSQLite)
library(temizhavaR)
library(readxl)
library(stringr)
library(dplyr)
library(dbplyr)

# Update if new station is selected, zaman aralığı da seçsin, overwrite, veri varsa o zaman aralığındaki her şeyi sil.
# excelleri servera at, serverda çalıştır
# home/basak
# checkleri çalıştır


read_and_write_data <- function(delete_previous = FALSE, pattern) {
  db <- create_postgres_conn()
  
   if (delete_previous) {
    dbExecute(db, "DELETE FROM daily_detail")
    dbExecute(db, "DELETE FROM hourly_detail")
    cat("Previous data cleared from tables\n")
  }
  col_mapping <- c(
    "PM2.5" = "PM25"
  )

  location_tbl <- tbl(db, "location")
  daily_tbl <- tbl(db, "daily_detail")
  hourly_tbl <- tbl(db, "hourly_detail")
  
  data_dir <- "./TemizHava_raw_data"
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
          Tarih = as.POSIXct("1900-01-01", tz="UTC") + 
            (as.numeric(Tarih) - 2) * 86400
        )
      
      df
    }, error = function(e) {
      cat("Error processing file:", file, "\n", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(raw_data)) next
    
    target_ref <- if(target_table == "daily_detail") daily_tbl else hourly_tbl
    min_date <- format(min(raw_data$Tarih), "%Y-%m-%d %H:%M:%S")
    
    existing_count <- target_ref %>%
      filter(
        Istasyon_modified == station_extracted,
        date_trunc('day', Tarih) == date_trunc('day', sql(paste0("'", min_date, "'::timestamptz")))
      ) %>%
      count() %>%
      collect() %>%
      pull(n)
    
    if (existing_count > 0) {
      cat("Data already exists for station:", station_extracted, "\n")
      next
    }

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
        across(where(is.character), ~ifelse(is.na(.) | . == "" | . == "-", NA, .)),
        Tarih = format(Tarih, "%Y-%m-%d %H:%M:%S")
      ) %>%
      select(all_of(expected_cols))
    
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

# rows_written <- read_and_write_data()
data_dir <- "./TemizHava_raw_data/Adana/"
# rows_written <- read_and_write_data(pattern = "Adana-Seyhan_saatlik_detay_2014-2024.xlsx")
rows_written <- read_and_write_data(pattern = "\\.xlsx$")

# add to readme

cat("Total rows written:", rows_written, "\n")
