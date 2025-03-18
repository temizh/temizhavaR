library(DBI)
library(dplyr)
library(lubridate)
library(writexl)
library(temizhavaR)

threshold <- 75    
total_days <- 365

conn <- create_postgres_conn()

cities <- dbGetQuery(conn, 'SELECT DISTINCT "Sehir" FROM location')$Sehir

years <- dbGetQuery(conn, 'SELECT DISTINCT EXTRACT(YEAR FROM "Tarih")::int AS year FROM daily_detail')$year

details_results <- list()
summary_results <- list()

for(year in years) {
  for(city in cities) {
    stations_query <- sprintf("SELECT \"Istasyon_modified\" FROM location WHERE \"Sehir\" = '%s'", city)
    stations <- dbGetQuery(conn, stations_query)$Istasyon_modified
    if(length(stations) == 0) next
    stations_str <- paste0("'", paste(stations, collapse = "','"), "'")
    
    query <- sprintf("SELECT \"Istasyon_modified\", \"Tarih\", \"PM25\", \"PM10\" FROM daily_detail 
                      WHERE \"Istasyon_modified\" IN (%s) AND \"Tarih\" BETWEEN '%s-01-01' AND '%s-12-31'",
                      stations_str, year, year)
    data <- dbGetQuery(conn, query)
    if(nrow(data) == 0) next
    
    data <- data %>%
      mutate(Tarih = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S"))
    
    station_summary <- data %>%
      group_by(Istasyon_modified) %>%
      summarise(
        pm25_count = sum(!is.na(PM25)),
        pm10_count = sum(!is.na(PM10)),
        pm25_avg = mean(PM25, na.rm = TRUE),
        pm10_avg = mean(PM10, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        pm25_percentage = (pm25_count / total_days) * 100,
        pm10_percentage = (pm10_count / total_days) * 100,
        derived_pm25 = if_else(pm25_percentage < threshold & pm10_percentage >= threshold, pm10_avg * 0.6667, NA_real_),
        final_pm25 = case_when(
          pm25_percentage >= threshold ~ pm25_avg,
          pm10_percentage >= threshold ~ pm10_avg * 0.6667,
          TRUE ~ NA_real_
        )
      ) %>%
      filter(!is.na(final_pm25)) %>%
      mutate(
        Istasyon = Istasyon_modified,
        PM25_veri_yüzdesi = round(pm25_percentage, 2),
        PM10_veri_yüzdesi = round(pm10_percentage, 2),
        Ölçülen_PM25 = round(pm25_avg, 2),
        Ölçülen_PM10 = round(pm10_avg, 2),
        `Pm10’dan_türetilen_PM25` = if_else(pm25_percentage < threshold & pm10_percentage >= threshold, round(pm10_avg * 0.6667, 2), NA_real_),
        Nihai_PM25 = round(final_pm25, 2),
        Year = year,
        Sehir = city
      ) %>%
      select(Year, Sehir, Istasyon, PM25_veri_yüzdesi, PM10_veri_yüzdesi, Ölçülen_PM25, Ölçülen_PM10, `Pm10’dan_türetilen_PM25`, Nihai_PM25)
    
    details_results[[length(details_results) + 1]] <- station_summary
    
    overall_pm25 <- mean(station_summary$Nihai_PM25, na.rm = TRUE)
    summary_results[[length(summary_results) + 1]] <- data.frame(
      Year = year,
      Sehir = city,
      Nihai_PM25_Ortalama = round(overall_pm25, 2)
    )
  }
}

details_df <- dplyr::bind_rows(details_results)
summary_df <- dplyr::bind_rows(summary_results)

disconnect_postgres(conn)

output_file <- file.path(getOption("temizhavaR.base_dir"), "__results", "city_PM25_all_years.xlsx")
write_xlsx(list(Details = details_df, Summary = summary_df), output_file)

print("Excel output created successfully")
