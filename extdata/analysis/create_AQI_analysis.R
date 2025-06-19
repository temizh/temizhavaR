# Description: This script creates AQI analysis
create_AQI_analysis <- function(start_year, end_year, schema_name) {
  message("Creating AQI analysis...")

  # Configuration
  parameters <- c("PM10", "PM25", "SO2", "NO2", "O3", "CO")

  # Database
  message("Connecting to the database...")
  con <- create_postgres_conn()

  # Ensure schema exists (create if not)
  dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, schema_name)))

  data <- tbl(con, "hourly_detail_zcleaned")

  # Prepare data
  data <- data %>%
    rename(Istasyon = "Istasyon_modified") %>%
    select(-Tarih) %>%
    rename(Tarih = "Tarih_ist") %>%
    mutate(Yıl = year(Tarih)) %>%
    filter(Yıl >= start_year & Yıl <= end_year) %>%
    filter(Istasyon %in% c("Eskişehir-Vişnepark"))

  # Analyse data
  message("Analysing data...")

  analysed_data <- data %>%
    select(Istasyon, Tarih)

  # Calculate AQI
  for (parameter in parameters) {
    message("Calculating AQI for ", parameter, "...")

    col_name = paste0(parameter, "_AQI")

    tmp <- data %>% 
      calculate_AQI(parameter) %>%
      rename(!!col_name := AQI)

    analysed_data <- left_join(analysed_data, tmp, by = c("Istasyon", "Tarih"))
  }

  # Save data
  message("Saving data...")
  compute(analysed_data, in_schema(schema_name, "aqi_analysis"), temporary = FALSE)

  # Close conection
  dbDisconnect(con)
  message("AQI analysis are created.")
}

create_AQI_analysis(2014, 2024, "aqi_new")