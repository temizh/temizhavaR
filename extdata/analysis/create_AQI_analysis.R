# Description: This script creates AQI analysis
create_AQI_analysis <- function(start_year, end_year, schema_name) {
  message("Creating AQI analysis...")

  # Configuration
  parameters <- c("PM10", "PM25", "SO2", "NO2", "O3", "CO")

  # Database
  message("Connecting to the database...")
  con <- create_postgres_conn()

  data <- tbl(con, "hourly_detail")

  # Prepare data
  data <- data %>%
    rename(Istasyon = "Istasyon_modified") %>%
    mutate(Yıl = year(Tarih)) %>%
    filter(Yıl >= start_year & Yıl <= end_year)

  # Analyse data
  message("Analysing data...")

  analysed_data <- data %>%
    select(Istasyon, Tarih)

  # Calculate AQI
  for (parameter in parameters) {
    message("Calculating AQI for ", parameter, "...")

    tmp <- data %>% 
      calculate_AQI(parameter)

    analysed_data <- left_join(analysed_data, tmp, by = c("Istasyon", "Tarih"))    
  }

  # Save data
  message("Saving data...")
  compute(analysed_data, in_schema(schema_name, "aqi_analysis"), temporary = FALSE)

  # Close conection
  dbDisconnect(con)
  message("AQI analysis are created.")
}