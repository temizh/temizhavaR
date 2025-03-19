# Description: This script creates intermediate analysis for daily data.
create_daily_intermediate_analysis <- function(start_year, end_year, schema_name) {
  message("Creating intermediate analysis for daily data...")

  # Configuration
  data_in_year <- 365 # for daily data

  # Database
  message("Connecting to the database...")
  con <- create_postgres_conn()

  data <- tbl(con, "daily_detail")

  # Prepare data
  data <- data %>%
    rename(Istasyon = "Istasyon_modified") %>%
    mutate(Yıl = year(Tarih)) %>%
    mutate(Ay = month(Tarih)) %>%
    mutate(Gün = day(Tarih)) %>%
    filter(Yıl >= start_year & Yıl <= end_year)

  # Group data
  grouped_data <- data %>%
    group_by(Istasyon, Yıl)

  # Analyse data
  message("Analysing data...")

  # PM10 ----------------------------
  PM10_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("PM10", data_in_year) %>%
    rename("PM10_Veri_Mevcudiyeti" = result)

  PM10_Ortalaması <- grouped_data %>%
    calculate_average("PM10") %>%
    rename("PM10_Ortalaması" = result)

  PM10_45_Altı_Veri_Sayısı <- grouped_data %>%
    calculate_exceedance("PM10", 45, "below") %>%
    rename("PM10_45_Altı_Veri_Sayısı" = result)

  PM10_50_Üstü_Veri_Sayısı <- grouped_data %>%
    calculate_exceedance("PM10", 50, "above") %>%
    rename("PM10_50_Üstü_Veri_Sayısı" = result)

  # PM25 ----------------------------
  PM25_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("PM25", data_in_year) %>%
    rename("PM25_Veri_Mevcudiyeti" = result)

  PM25_Ortalaması <- grouped_data %>%
    calculate_average("PM25") %>%
    rename("PM25_Ortalaması" = result)

  PM25_15_Üstü_Veri_Sayısı <- grouped_data %>%
    calculate_exceedance("PM25", 15, "above") %>%
    rename("PM25_15_Üstü_Veri_Sayısı" = result)

  PM25_15_Altı_Veri_Sayısı <- grouped_data %>%
    calculate_exceedance("PM25", 15, "below") %>%
    rename("PM25_15_Altı_Veri_Sayısı" = result)

  PM25_Tamamlanmış <- grouped_data %>%
    estimate_PM25(data_in_year) %>%
    rename("PM25_Tamamlanmış" = result)

  # SO2 ----------------------------
  SO2_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("SO2", data_in_year) %>%
    rename("SO2_Veri_Mevcudiyeti" = result)

  SO2_Ortalaması <- grouped_data %>%
    calculate_average("SO2") %>%
    rename("SO2_Ortalaması" = result)

  SO2_40_Üstü_Veri_Sayısı <- grouped_data %>%
    calculate_exceedance("SO2", 40, "above") %>%
    rename("SO2_40_Üstü_Veri_Sayısı" = result)

  SO2_125_Üstü_Veri_Sayısı <- grouped_data %>%
    calculate_exceedance("SO2", 125, "above") %>%
    rename("SO2_125_Üstü_Veri_Sayısı" = result)

  # NO2 ----------------------------
  NO2_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("NO2", data_in_year) %>%
    rename("NO2_Veri_Mevcudiyeti" = result)

  NO2_Ortalaması <- grouped_data %>%
    calculate_average("NO2") %>%
    rename("NO2_Ortalaması" = result)

  NO2_25_Üstü_Veri_Sayısı <- grouped_data %>%
    calculate_exceedance("NO2", 25, "above") %>%
    rename("NO2_25_Üstü_Veri_Sayısı" = result)

  # NOX ----------------------------
  NOX_Veri_Mevcudiyeti <- grouped_data %>% 
    calculate_percentage("NOX", data_in_year) %>%
    rename("NOX_Veri_Mevcudiyeti" = result)

  NOX_Ortalaması <- grouped_data %>%
    calculate_average("NOX") %>%
    rename("NOX_Ortalaması" = result)

  # O3 ----------------------------
  O3_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("O3", data_in_year) %>%
    rename("O3_Veri_Mevcudiyeti" = result)

  O3_Ortalaması <- grouped_data %>%
    calculate_average("O3") %>%
    rename("O3_Ortalaması" = result)

  O3_Kış_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("O3", data_in_year = 182, season = c(1, 2, 3, 10, 11, 12)) %>%
    rename("O3_Kış_Veri_Mevcudiyeti" = result)

  O3_Yaz_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("O3", data_in_year = 183, season = c(4, 5, 6, 7, 8, 9)) %>%
    rename("O3_Yaz_Veri_Mevcudiyeti" = result)

  # CO ----------------------------
  CO_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_percentage("CO", data_in_year) %>%
    rename("CO_Veri_Mevcudiyeti" = result)

  CO_Ortalaması <- grouped_data %>%
    calculate_average("CO") %>%
    rename("CO_Ortalaması" = result)

  # Combine results
  message("Combining results...")
  analysed_data <- PM10_Veri_Mevcudiyeti %>% # PM10 ----------------------------
    left_join(PM10_Ortalaması, by = c("Istasyon", "Yıl")) %>%
    left_join(PM10_45_Altı_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    left_join(PM10_50_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%

    # PM25 ----------------------------
    left_join(PM25_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    left_join(PM25_Ortalaması, by = c("Istasyon", "Yıl")) %>%
    left_join(PM25_15_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    left_join(PM25_15_Altı_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    left_join(PM25_Tamamlanmış, by = c("Istasyon", "Yıl")) %>%

    # SO2 ----------------------------
    left_join(SO2_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    left_join(SO2_Ortalaması, by = c("Istasyon", "Yıl")) %>%
    left_join(SO2_40_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    left_join(SO2_125_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%

    # NO2 ----------------------------
    left_join(NO2_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    left_join(NO2_Ortalaması, by = c("Istasyon", "Yıl")) %>%
    left_join(NO2_25_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%

    # NOX ----------------------------
    left_join(NOX_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    left_join(NOX_Ortalaması, by = c("Istasyon", "Yıl")) %>%

    # O3 ----------------------------
    left_join(O3_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    left_join(O3_Ortalaması, by = c("Istasyon", "Yıl")) %>%
    left_join(O3_Kış_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    left_join(O3_Yaz_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%

    # CO ----------------------------
    left_join(CO_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    left_join(CO_Ortalaması, by = c("Istasyon", "Yıl"))

  # Save data
  message("Saving data...")
  compute(analysed_data, in_schema(schema_name, "daily_intermediate_analysis"), temporary = FALSE)

  # Close conection
  dbDisconnect(con)
  message("Intermediate analysis for daily data is created.")
}