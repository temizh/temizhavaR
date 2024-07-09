library(writexl)

create_location_table_2022 <- function() {

  YEAR <- options()$temizhavaR.YEAR

  #Get 2022 station saatlik/gunluk list
  stopifnot(grepl("2022", raw_dir))
  saatlik_files2022 <- list.files(pattern = "_saatlik_detay_", recursive = TRUE)
  saatlik_stations_2022 <- sapply(strsplit(saatlik_files2022, "/"), function(x) x[[2]])
  saatlik_stations_2022 <- sapply(strsplit(saatlik_stations_2022, "_saatlik"), function(x) x[[1]])
  gunluk_files2022 <- list.files(pattern = "_gunluk_detay_", recursive = TRUE)
  gunluk_stations_2022 <- sapply(strsplit(gunluk_files2022, "/"), function(x) x[[2]])
  gunluk_stations_2022 <- sapply(strsplit(gunluk_stations_2022, "_gunluk"), function(x) x[[1]])
  gunlukler_2022 <- data.frame(Istasyonlar = gunluk_stations_2022, gunluk_files2022 = gunluk_files2022)
  saatlikler_2022 <- data.frame(Istasyonlar = saatlik_stations_2022, saatlik_files2022 = saatlik_files2022)
  veriler_2022 <- left_join(gunlukler_2022, saatlikler_2022, by = join_by(Istasyonlar))
  veriler_2022$Istasyonlar <- gsub(" \\(yeni\\)", "", veriler_2022$Istasyonlar)
  veriler_2022$Istasyonlar <- gsub(" \\(Yeni\\) ", "", veriler_2022$Istasyonlar)
  veriler_2022$Istasyonlar <- gsub(" \\(Yeni\\)", "", veriler_2022$Istasyonlar)
  veriler_2022$Istasyonlar <- gsub("SunayPark", "Sunaypark", veriler_2022$Istasyonlar)
  veriler_2022$Istasyonlar <- gsub(" - ", "-", veriler_2022$Istasyonlar)
  veriler_2022$Istasyonlar <- gsub("- ", "-", veriler_2022$Istasyonlar)

  # Check if all of the files DO exist on disk
  stopifnot(all(sapply(veriler_2022$gunluk_files2022, function(x) file.exists(x))))
  stopifnot(all(sapply(veriler_2022$saatlik_files2022, function(x) file.exists(x))))

  #Read 2023 location table and use it as template
  raw_dir_2023 <- gsub("2022", "2023", raw_dir)
  stopifnot(grepl("2023", raw_dir_2023))
  #Get 2023 location list
  mydb2023 <- dbConnect(RSQLite::SQLite(), file.path(raw_dir_2023,"temiz-hava.sqlite"))
  location2023 <- dbReadTable(mydb2023, paste0("location_2023"))

  ###############################
  #Remove spaces from station lists
  location2023$Istasyonlar <- gsub(" - ", "-", location2023$Istasyonlar)
  location2023$Istasyonlar <- gsub("\\/", " ", location2023$Istasyonlar)
  dbDisconnect(mydb2023)

  location2022 <- left_join(veriler_2022 %>% select(Istasyonlar, gunluk_files2022, saatlik_files2022), location2023, by = join_by(Istasyonlar)) %>%
    relocate(gunluk_files2022, .after=Id) %>%
    relocate(saatlik_files2022, .after=gunluk_files2022)

  #Save to see which 2022 stations are no more in 2023 database
  write_xlsx(location2022, path = "location_table_2022_w_missing_2023.xlsx", col_names = TRUE)

  # Include Konya Sarayönü Arkaplan (Does not have name Konya in it)
  sarayonu_idx <- grep("Sarayönü Arkaplan", location2022$Istasyonlar)
  location2022$Sehir[sarayonu_idx] <- "Konya"

   #Cycle through all cities, create new uuids for 2022 stations that were not on
  # the 2023 database or that are incomplete
  u_cities <- unique(location2023$Sehir)

  I <- 1

  for (mycity in u_cities) {
    missing_idx <- which(is.na(location2022$Bolge) & grepl(mycity, location2022$Istasyonlar))

    print(paste("I:", I, mycity))
    #if (I==73) browser()
    # Include Konya Sarayönü Arkaplan (Does not have name Konya in it)
    sarayonu_idx <- grep("Sarayönü", location2022$Istasyonlar)
    if (mycity == "Konya" & length(sarayonu_idx)!=0) {
      missing_idx <- c(missing_idx, sarayonu_idx)
    }

    if(length(missing_idx)>0){
      non_missing_idx <- which(!is.na(location2022$Bolge) & grepl(mycity, location2022$Istasyonlar))
      location2022$Bolge[missing_idx] <- location2022$Bolge[non_missing_idx[1]]
      location2022$Sehir[missing_idx] <- location2022$Sehir[non_missing_idx[1]]
      location2022$Plaka[missing_idx] <- location2022$Plaka[non_missing_idx[1]]
      for (my_missing_idx in missing_idx)
        location2022$Id[my_missing_idx] <- uuid::UUIDgenerate()
    }
    I <- I+1
  }

  YEAR <- options()$temizhavaR.YEAR
  mydb <- dbConnect(RSQLite::SQLite(), file.path(raw_dir,"temiz-hava.sqlite"))
  dbExecute(mydb, "DELETE FROM location_2022")
  dbWriteTable(mydb, paste0("location_", YEAR), location2022, append = TRUE, row.names = FALSE)
  dbDisconnect(mydb)

  write_xlsx(location2022, path = "location_table_2022.xlsx", col_names = TRUE)
  print(paste("Wrote", file.path(getwd(), "location_table_2022.xlsx") ))
  location2022
}
