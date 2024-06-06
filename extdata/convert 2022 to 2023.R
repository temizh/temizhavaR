library(readxl)
library(writexl)
library(dplyr)

setwd("/home/acizmeli/Documents/KaraRaporu/HamVeriler_2022/")

cities <- list.dirs()
cities <- gsub("\\.\\/", "", cities)
cities <- cities[-1]

#mycolnames <- c("PM10", "PM25", "SO2", "NO2", "NOX", "NO", "O3")

read_and_convert_2022_city_file <- function(my_city, infile) {

  fileheader <- read_xlsx(file.path(my_city, infile), n_max = 1, .name_repair = "unique_quiet")
  fileheader <- as.data.frame(fileheader)
  fileheader[1,] <- colnames(fileheader)

  station_idx <- grep(my_city, colnames(fileheader))
  all_stations <- colnames(fileheader)[station_idx]
  all_stations <- gsub("/", " ", all_stations)

  fullDF <- read_xlsx(file.path(my_city, infile), .name_repair = "unique_quiet")
  colnames_all <- as.character(fullDF[1,])
  colnames_all[1] <- "Tarih"
  fullDF <- fullDF[-1,]
  fullDF <- as.data.frame(fullDF)

  for (I in 1:length(all_stations)) {
    #I<-1
    upper_limit <- station_idx[I+1]
    if (is.na(upper_limit))
      upper_limit <- ncol(fullDF)+1

    col_indexes <- c(1, station_idx[I]:(upper_limit-1))
    statDF <- fullDF[, col_indexes]
    mycolnames <- colnames_all[col_indexes]

    statDF[, 1] <- format(statDF[, 1], format = "%d.%m.%Y %H:%M:%S")

    first_line <- rep("", ncol(statDF))
    first_line[2] <- all_stations[I]
    first_line <- data.frame(t(first_line))
    first_line[2, ] <- mycolnames
    colnames(statDF) <- colnames(first_line)

    statDF <- rbind(first_line, statDF)
    statDF[2,1] <- "Tarih"

    out_xlsx_filename <- paste0(my_city, "/", all_stations[I], "_gunluk_detay_2022.xlsx")
    if (grepl("saatlik", infile))
      out_xlsx_filename <- paste0(my_city, "/", all_stations[I], "_saatlik_detay_2022.xlsx")

    write_xlsx(
      statDF,
      path = out_xlsx_filename,
      col_names = FALSE,
      format_headers = TRUE,
      use_zip64 = FALSE
    )
    print(paste("Wrote", out_xlsx_filename))
  }
}

for (my_city in cities) {
  #my_city <- cities[38]
  pattern_gunluk <- "günlük_detaylı"
  pattern_saatlik <- "saatlik_detaylı"

  myfiles <- list.files(path = my_city, pattern = pattern_gunluk)
  for (infile in myfiles)
    read_and_convert_2022_city_file(my_city, infile)

  myfiles <- list.files(path = my_city, pattern = pattern_saatlik)
  for (infile in myfiles)
    read_and_convert_2022_city_file(my_city, infile)
}
