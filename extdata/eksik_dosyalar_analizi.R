mydir <- "/home/acizmeli/Documents/KaraRaporu/HamVeriler_2023"

setwd(mydir)

mydb <- dbConnect(RSQLite::SQLite(), file.path(mydir,"temiz-hava.sqlite"))

full_stations <- dbGetQuery(mydb, "SELECT * FROM location_2023")
all_daily <- dbGetQuery(mydb, "SELECT * FROM daily_detail")
all_hourly <- dbGetQuery(mydb, "SELECT * FROM hourly_detail")

dbDisconnect(mydb)

all_stations <- sort(unique(full_stations$Istasyonlar))
all_stations <- tibble(Istasyon=all_stations)

u_stations_daily <- sort(unique(all_daily$Istasyon))
u_stations_hourly <- sort(unique(all_hourly$Istasyon))

daily_summary <- all_daily %>%
  select(Istasyon, PM10, PM2.5, SO2, CO, NO2, NOX, NO, O3) %>%
  group_by(Istasyon) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  rename(PM10_daily=PM10, PM2.5_daily=PM2.5, SO2_daily=SO2, CO_daily=CO, NO2_daily=NO2, NOX_daily=NOX, NO_daily=NO, O3_daily=O3)

hourly_summary <- all_hourly %>%
  select(Istasyon, PM10, PM2.5, SO2, CO, NO2, NOX, NO, O3) %>%
  group_by(Istasyon) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  rename(PM10_hourly=PM10, PM2.5_hourly=PM2.5, SO2_hourly=SO2, CO_hourly=CO, NO2_hourly=NO2, NOX_hourly=NOX, NO_hourly=NO, O3_hourly=O3)

all_summary <- left_join(all_stations, daily_summary, by = join_by(Istasyon)) %>%
  print(n=332)

all_summary <- left_join(all_summary, hourly_summary, by = join_by(Istasyon)) %>%
  relocate(PM10_hourly, .after=PM10_daily) %>%
  relocate(PM2.5_hourly, .after=PM2.5_daily) %>%
  relocate(SO2_hourly, .after=SO2_daily) %>%
  relocate(CO_hourly, .after=CO_daily) %>%
  relocate(NO2_hourly, .after=NO2_daily) %>%
  relocate(NOX_hourly, .after=NOX_daily) %>%
  relocate(NO_hourly, .after=NO_daily) %>%
  relocate(O3_hourly, .after=O3_daily)

analiz_output_file <- file.path(mydir, "__results", "eksik_dosyalar_analizi.xlsx")

write_xlsx(
  all_summary,
  path = analiz_output_file,
  col_names = TRUE,
  format_headers = TRUE
)
