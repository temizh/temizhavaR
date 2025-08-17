# Description: This script creates AQI analysis
create_AQI_analysis <- function(start_year, end_year, schema_name) {
  message("Creating AQI analysis...")

  # Configuration
  parameters <- c("PM10", "PM25", "SO2", "NO2", "O3")

  # Database
  message("Connecting to the database...")
  con <- create_postgres_conn()

  # Ensure schema exists (create if not)
  dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, schema_name)))

  data <- tbl(con, "hourly_detail_zcleaned")

  stations <- tbl(con, "location") %>%
    select(Istasyon_modified, Sehir) %>%
    rename(Istasyon = Istasyon_modified)

  # Prepare data
  data <- data %>%
    rename(Istasyon = "Istasyon_modified") %>%
    rename(Tarih = "Tarih_NOTZ") %>%
    mutate(Yıl = year(Tarih)) %>%
    # mutate(Season = case_when(
    #   month(Tarih) %in% c(12, 1, 2) ~ "Winter",
    #   month(Tarih) %in% c(3, 4, 5) ~ "Spring",
    #   month(Tarih) %in% c(6, 7, 8) ~ "Summer",
    #   month(Tarih) %in% c(9, 10, 11) ~ "Autumn",
    #   TRUE ~ NA_character_
    # )) %>%
    filter(Yıl >= start_year & Yıl <= end_year) #%>%
    #filter(Istasyon %in% c("Adana-Valilik", "Ankara-KeçiörenSanatoryum", "Antalya-Kepez", "Edirne", "Edirne-Keşan-MTHM", "Erzurum-Aziziye", "İstanbul-Başakşehir-MTHM", "İstanbul-Ümraniye", "İstanbul-Ümraniye-MTHM", "İzmir-BornovaİBB", "İzmir-GüzelyalıİBB", "Şanlıurfa", "Sivas-Başöğretmen"))

  # Analyse data
  message("Analysing data...")

  analysed_data <- data %>%
    select(Istasyon, Tarih, Yıl)

  # Calculate AQI
  for (parameter in parameters) {
    message("Calculating AQI for ", parameter, "...")

    col_name = paste0(parameter, "_AQI")

    tmp <- data %>% 
      calculate_AQI(parameter) %>%
      rename(!!col_name := AQI)

    analysed_data <- left_join(analysed_data, tmp, by = c("Istasyon", "Tarih"))
  }

  analysed_data <- analysed_data %>%
    mutate(
      AQI_max = pmax(
        PM10_AQI, PM25_AQI, SO2_AQI, NO2_AQI, O3_AQI, #CO_AQI,
        na.rm = TRUE
      ),
      AQI_max_pollutant = case_when(
        PM10_AQI == AQI_max ~ "PM10",
        PM25_AQI == AQI_max ~ "PM25",
        SO2_AQI == AQI_max ~ "SO2",
        NO2_AQI == AQI_max ~ "NO2",
        O3_AQI == AQI_max ~ "O3",
        #CO_AQI == AQI_max ~ "CO",
        TRUE ~ NA_character_
      )
    )

  yearly_data <- analysed_data %>%
    group_by(Yıl, Istasyon) %>%
    summarise(
      AQI_average = mean(AQI_max, na.rm = TRUE),
      PM10_AQI_contribution = mean(ifelse(AQI_max_pollutant == "PM10", 1, 0)) * 100,
      PM25_AQI_contribution = mean(ifelse(AQI_max_pollutant == "PM25", 1, 0)) * 100,
      SO2_AQI_contribution = mean(ifelse(AQI_max_pollutant == "SO2", 1, 0)) * 100,
      NO2_AQI_contribution = mean(ifelse(AQI_max_pollutant == "NO2", 1, 0)) * 100,
      O3_AQI_contribution = mean(ifelse(AQI_max_pollutant == "O3", 1, 0)) * 100,
      #CO_AQI_contribution = mean(ifelse(AQI_max_pollutant == "CO", 1, 0)) * 100,
    ) %>%
    mutate(
      # compare contribution columns and then select the most pollutant
      max_AQI = pmax(
        PM10_AQI_contribution, PM25_AQI_contribution, SO2_AQI_contribution,
        NO2_AQI_contribution, O3_AQI_contribution, #CO_AQI_contribution,
        na.rm = TRUE
      ),
      AQI_most_pollutant = case_when(
        PM10_AQI_contribution == max_AQI ~ "PM10",
        PM25_AQI_contribution == max_AQI ~ "PM25",
        SO2_AQI_contribution == max_AQI ~ "SO2",
        NO2_AQI_contribution == max_AQI ~ "NO2",
        O3_AQI_contribution == max_AQI ~ "O3",
        #CO_AQI_contribution == max_AQI ~ "CO",
        TRUE ~ NA_character_
      )
    )

  yearly_city_data <- yearly_data %>%
    ungroup() %>%
    left_join(stations, by = "Istasyon") %>%
    group_by(Sehir, Yıl) %>%
    summarise(
      AQI_average = mean(AQI_average, na.rm = TRUE),
      PM10_AQI_contribution = mean(PM10_AQI_contribution, na.rm = TRUE),
      PM25_AQI_contribution = mean(PM25_AQI_contribution, na.rm = TRUE),
      SO2_AQI_contribution = mean(SO2_AQI_contribution, na.rm = TRUE),
      NO2_AQI_contribution = mean(NO2_AQI_contribution, na.rm = TRUE),
      O3_AQI_contribution = mean(O3_AQI_contribution, na.rm = TRUE),
      #CO_AQI_contribution = mean(CO_AQI_contribution, na.rm = TRUE)
    ) %>%
    mutate(
      # compare contribution columns and then select the most pollutant
      max_AQI = pmax(
        PM10_AQI_contribution, PM25_AQI_contribution, SO2_AQI_contribution,
        NO2_AQI_contribution, O3_AQI_contribution, #CO_AQI_contribution,
        na.rm = TRUE
      ),
      AQI_most_pollutant = case_when(
        PM10_AQI_contribution == max_AQI ~ "PM10",
        PM25_AQI_contribution == max_AQI ~ "PM25",
        SO2_AQI_contribution == max_AQI ~ "SO2",
        NO2_AQI_contribution == max_AQI ~ "NO2",
        O3_AQI_contribution == max_AQI ~ "O3",
        #CO_AQI_contribution == max_AQI ~ "CO",
        TRUE ~ NA_character_
      )
    )


  # Save data
  message("Saving data...")
  compute(analysed_data, in_schema(schema_name, "aqi_analysis"), temporary = FALSE)
  message("A")
  compute(yearly_data, in_schema(schema_name, "aqi_yearly_analysis"), temporary = FALSE)
  message("B")
  compute(yearly_city_data, in_schema(schema_name, "aqi_yearly_city_analysis"), temporary = FALSE)
  message("C")

  # Close conection
  dbDisconnect(con)
  message("AQI analysis are created.")
}

create_AQI_analysis(2024, 2024, "aqi_2024_13082025")