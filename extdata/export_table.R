library(temizhavaR)
library(dplyr)
library(dbplyr)
library(openxlsx)

conn <- create_postgres_conn()

# get data from the shema aqi_new and aqi-analysis table
data <- tbl(conn, in_schema("aqi_2024_13082025", "aqi_yearly_city_analysis"))

#stations <- tbl(conn, "location") %>% collect() %>% pull(Istasyon_modified)
#stations <- c("Antalya-Kepez", "Edirne", "Edirne-Keşan-MTHM", "Erzurum-Aziziye", "İstanbul-Başakşehir-MTHM","İstanbul-Ümraniye", "İzmir-BornovaİBB", "Sivas-Başöğretmen")

# for (station in stations) {
#   print(paste0("Processing station: ", station))

#   station_data <- data %>%
#     filter(Istasyon == station) %>%
#     #arrange(Tarih) %>%
#     collect()

#   # if station data is empty, skip to next station
#   if (nrow(station_data) == 0) {
#     print(paste0("No data found for station: ", station))
#     next
#   }
  
#   # write to csv
#   file_name <- paste0("extdata/station_exports/", gsub(" ", "_", station), ".csv")
#   write.csv(station_data, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")

#   print(paste0("Exported data for station: ", station))
# }

file_name <- paste0("extdata/exports/aqi_yearly_city_2024.xlsx")
write.xlsx(data, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")