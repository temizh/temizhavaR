library(temizhavaR)
library(dplyr)
library(dbplyr)

conn <- create_postgres_conn()

data <- tbl(conn, "hourly_detail_zcleaned")

stations <- tbl(conn, "location") %>% collect() %>% pull(Istasyon_modified)

for (station in stations) {
  print(paste0("Processing station: ", station))

  station_data <- data %>%
    filter(Istasyon_modified == station) %>%
    collect()
  
  # write to csv
  file_name <- paste0("extdata/exports/", gsub(" ", "_", station), ".csv")
  write.csv(station_data, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")

  print(paste0("Exported data for station: ", station))
}