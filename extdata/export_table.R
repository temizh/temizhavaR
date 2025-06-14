library(temizhavaR)
library(dplyr)
library(dbplyr)
library(lubridate)

conn <- create_postgres_conn()

data <- tbl(conn, "hourly_detail_zcleaned")

stations <- tbl(conn, "location") %>% collect() %>% pull(Istasyon_modified)

for (station in stations) {
  station_data <- data %>%
    filter(Istasyon_modified == station) %>%
    collect() %>%
    mutate(Tarih = lubridate::with_tz(Tarih, "Europe/Istanbul")) %>%
    arrange(Tarih)

  print(head(station_data))
  
  # write to csv
  file_name <- paste0("extdata/exports/", gsub(" ", "_", station), ".csv")
  write.csv(station_data, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")

}