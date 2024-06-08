library(writexl)


#Get 2022 station saatlik/gunluk list
stopifnot(grepl("2022", raw_dir))
saatlikler_2022 <- list.files(pattern = "_saatlik_detay_", recursive = TRUE)
saatlikler_2022 <- sapply(strsplit(saatlikler_2022, "/"), function(x) x[[2]])
saatlikler_2022 <- sapply(strsplit(saatlikler_2022, "_saatlik"), function(x) x[[1]])
gunlukler_2022 <- list.files(pattern = "_gunluk_detay_", recursive = TRUE)
gunlukler_2022 <- sapply(strsplit(gunlukler_2022, "/"), function(x) x[[2]])
gunlukler_2022 <- sapply(strsplit(gunlukler_2022, "_gunluk"), function(x) x[[1]])
gunlukler_2022 <- data.frame(Istasyon = gunlukler_2022, gunlukler_2022 = TRUE)
saatlikler_2022 <- data.frame(Istasyon = saatlikler_2022, saatlikler_2022 = TRUE)
veriler_2022 <- left_join(gunlukler_2022, saatlikler_2022, by = join_by(Istasyon))

write_xlsx(
  veriler_2022,
  path = "../veriler_2022.xlsx",
  col_names = TRUE,
  format_headers = TRUE
)

#Get 2023 location data
raw_dir_2023 <- gsub("2022", "2023", raw_dir)
stopifnot(grepl("2023", raw_dir_2023))
#Get 2023 location list
mydb2023 <- dbConnect(RSQLite::SQLite(), file.path(raw_dir_2023,"temiz-hava.sqlite"))
location2023 <- dbReadTable(mydb2023, paste0("location_2023"))

full_stations_2023 <- dbGetQuery(mydb2023, "SELECT * FROM location_2023")
all_daily <- dbGetQuery(mydb2023, "SELECT * FROM daily_detail")
all_hourly <- dbGetQuery(mydb2023, "SELECT * FROM hourly_detail")

dbDisconnect(mydb2023)

all_stations <- sort(unique(full_stations_2023$Istasyonlar))
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

allstations_2023 <- sort(unique(location2023$Istasyonlar))
allstations_2023 <- gsub("\\/", " ", allstations_2023)
allstations_2023 <- tibble(Istasyon=allstations_2023)
#Get 2023 station saatlik/gunluk list
setwd(raw_dir)
gunlukler_2023 <- list.files(pattern = "_gunluk_detay_", recursive = TRUE)
gunlukler_2023 <- sapply(strsplit(gunlukler_2023, "/"), function(x) x[[2]])
gunlukler_2023 <- sapply(strsplit(gunlukler_2023, "_gunluk"), function(x) x[[1]])
gunlukler_2023 <- data.frame(Istasyon = gunlukler_2023, gunlukler_2023 = TRUE)
saatlikler_2023 <- list.files(pattern = "_saatlik_detay_", recursive = TRUE, full.names = FALSE, include.dirs	= FALSE)
saatlikler_2023 <- sapply(strsplit(saatlikler_2023, "/"), function(x) x[[2]])
saatlikler_2023 <- sapply(strsplit(saatlikler_2023, "_saatlik"), function(x) x[[1]])
saatlikler_2023 <- data.frame(Istasyon = saatlikler_2023, saatlikler_2023 = TRUE)
veriler_2023 <- left_join(allstations_2023, gunlukler_2022, by = join_by(Istasyon))
veriler_2023 <- left_join(veriler_2023, saatlikler_2022, by = join_by(Istasyon))
init.temizhavaR()

write_xlsx(
  veriler_2023,
  path = "../veriler_2023.xlsx",
  col_names = TRUE,
  format_headers = TRUE
)

allstations_2023$Istasyon[239]==gunlukler_2023$Istasyon[237]
allstations_2023$Istasyon[239]==saatlikler_2023$Istasyon[237]
gunlukler_2023$Istasyon[237]==saatlikler_2023$Istasyon[237]


