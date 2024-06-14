library(writexl)
library(dplyr)

options(temizhavaR.basedir = "/home/acizmeli/Documents/KaraRaporu/HamVeriler_")
#YEAR <- options()$temizhavaR.YEAR

compare_yearly_means_from_daily_hourly <- function(YEAR) {
  init.temizhavaR()

  #Get location data
  #stopifnot(grepl(YEAR, raw_dir))

  datadir <- paste0(options()$temizhavaR.basedir, YEAR)
  stopifnot(dir.exists(datadir))

  mydb <- dbConnect(RSQLite::SQLite(), file.path(datadir,"temiz-hava.sqlite"))
  #Get location table
  full_stations <- dbGetQuery(mydb, paste0("SELECT * FROM location_", YEAR))

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

  all_summary <- left_join(all_stations, daily_summary, by = join_by(Istasyon))

  all_summary <- left_join(all_summary, hourly_summary, by = join_by(Istasyon)) %>%
    relocate(PM10_hourly, .after=PM10_daily) %>%
    relocate(PM2.5_hourly, .after=PM2.5_daily) %>%
    relocate(SO2_hourly, .after=SO2_daily) %>%
    relocate(CO_hourly, .after=CO_daily) %>%
    relocate(NO2_hourly, .after=NO2_daily) %>%
    relocate(NOX_hourly, .after=NOX_daily) %>%
    relocate(NO_hourly, .after=NO_daily) %>%
    relocate(O3_hourly, .after=O3_daily)

  analiz_output_file <- file.path(raw_dir, "__results", "compare_yearly_means_from_daily_hourly.xlsx")

  write_xlsx(
    all_summary,
    path = analiz_output_file,
    col_names = TRUE,
    format_headers = TRUE
  )

  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)
  p1 <- ggplot(data=all_summary,aes(x=PM10_hourly, y=PM10_daily)) + geom_point()
  p2 <- ggplot(data=all_summary,aes(x=PM2.5_hourly, y=PM2.5_daily)) + geom_point()
  p3 <- ggplot(data=all_summary,aes(x=SO2_hourly, y=SO2_daily)) + geom_point()
  p4 <- ggplot(data=all_summary,aes(x=CO_hourly, y=CO_daily)) + geom_point()
  p5 <- ggplot(data=all_summary,aes(x=NO2_hourly, y=NO2_daily)) + geom_point()
  p6 <- ggplot(data=all_summary,aes(x=NOX_hourly, y=NOX_daily)) + geom_point()
  p7 <- ggplot(data=all_summary,aes(x=NO_hourly, y=NO_daily)) + geom_point()
  p8 <- ggplot(data=all_summary,aes(x=O3_hourly, y=O3_daily)) + geom_point()
  tt<-grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=3)
}

#compare_yearly_means_from_daily_hourly("2022")
compare_yearly_means_from_daily_hourly("2023")

#location2023 <- dbReadTable(mydb, paste0("location_", YEAR))
# allstations_2023 <- sort(unique(location2023$Istasyonlar))
# allstations_2023 <- gsub("\\/", " ", allstations_2023)
# allstations_2023 <- tibble(Istasyon=allstations_2023)
# #Get 2023 station saatlik/gunluk list
# setwd(raw_dir)
# gunlukler_2023 <- list.files(pattern = "_gunluk_detay_", recursive = TRUE)
# gunlukler_2023 <- sapply(strsplit(gunlukler_2023, "/"), function(x) x[[2]])
# gunlukler_2023 <- sapply(strsplit(gunlukler_2023, "_gunluk"), function(x) x[[1]])
# gunlukler_2023 <- data.frame(Istasyon = gunlukler_2023, gunlukler_2023 = TRUE)
# saatlikler_2023 <- list.files(pattern = "_saatlik_detay_", recursive = TRUE, full.names = FALSE, include.dirs	= FALSE)
# saatlikler_2023 <- sapply(strsplit(saatlikler_2023, "/"), function(x) x[[2]])
# saatlikler_2023 <- sapply(strsplit(saatlikler_2023, "_saatlik"), function(x) x[[1]])
# saatlikler_2023 <- data.frame(Istasyon = saatlikler_2023, saatlikler_2023 = TRUE)
# veriler_2023 <- left_join(allstations_2023, gunlukler_2022, by = join_by(Istasyon))
# veriler_2023 <- left_join(veriler_2023, saatlikler_2022, by = join_by(Istasyon))
# init.temizhavaR()
#
# write_xlsx(
#   veriler_2023,
#   path = "../veriler_2023.xlsx",
#   col_names = TRUE,
#   format_headers = TRUE
# )


