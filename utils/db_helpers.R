library(DBI)

detect_and_fix_duplicates <- function(db_connection, table_name = "daily_detail") {
  dupes_query <- sprintf(
    'SELECT "Istasyon_modified", "Tarih", COUNT(*) as count
     FROM %s
     GROUP BY "Istasyon_modified", "Tarih"
     HAVING COUNT(*) > 1
     ORDER BY COUNT(*) DESC',
    table_name
  )
  
  duplicates <- dbGetQuery(db_connection, dupes_query)
  
  if (nrow(duplicates) > 0) {
    cat(sprintf("Found %d sets of duplicate records in %s\n", nrow(duplicates), table_name))
    print(head(duplicates, 10))
    
    for (i in 1:nrow(duplicates)) {
      station <- duplicates$Istasyon_modified[i]
      date <- duplicates$Tarih[i]
      
      records_query <- sprintf(
        'SELECT * FROM %s 
         WHERE "Istasyon_modified" = \'%s\' AND "Tarih" = \'%s\'',
        table_name, station, date
      )
      
      records <- dbGetQuery(db_connection, records_query)
      
      measurement_cols <- c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3")
      na_counts <- apply(records[, measurement_cols], 1, function(x) sum(is.na(x)))
      
      best_row_id <- which.min(na_counts)[1]  
      
      other_row_ids <- setdiff(1:nrow(records), best_row_id)
      if (length(other_row_ids) > 0) {
        for (row_id in other_row_ids) {
          delete_query <- sprintf(
            'DELETE FROM %s
             WHERE "Id" = %d',
            table_name, records$Id[row_id]
          )
          
          dbExecute(db_connection, delete_query)
        }
        
        cat(sprintf("Fixed duplicates for station %s on %s: kept 1, removed %d\n", 
                   station, date, length(other_row_ids)))
      }
    }
    
    return(nrow(duplicates))
  } else {
    cat(sprintf("No duplicates found in %s\n", table_name))
    return(0)
  }
}

fix_duplicates_in_db <- function(db_connection) {
  cat("Checking for duplicates in daily_detail...\n")
  daily_dupes <- detect_and_fix_duplicates(db_connection, "daily_detail")
  
  cat("Checking for duplicates in hourly_detail...\n")  
  hourly_dupes <- detect_and_fix_duplicates(db_connection, "hourly_detail")
  
  return(c(daily = daily_dupes, hourly = hourly_dupes))
}
