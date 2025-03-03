
library(dplyr)
library(readxl)
library(writexl)

compare_excel_with_db <- function(excel_file, db_connection, location_id, parameter) {
  # Read Excel data
  excel_data <- read_excel(excel_file)
  non_na_excel <- sum(!is.na(excel_data[[parameter]]))
  
  # Query DB data
  db_data <- dbGetQuery(db_connection, 
    sprintf("SELECT COUNT(*) as count FROM daily_detail 
            WHERE location_id = %d AND %s IS NOT NULL", location_id, parameter))
  
  # Compare counts
  report <- data.frame(
    source = c("Excel", "Database"),
    count = c(non_na_excel, db_data$count),
    difference = c(NA, non_na_excel - db_data$count)
  )
  
  return(report)
}
