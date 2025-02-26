library(readxl)
library(DBI)
library(RSQLite)
library(stringdist)

initializeDatabase <- function(db_path) {
    raw_dir <- getOption("temizhavaR.raw_dir")
    
    if (!is.null(raw_dir)) {
        setwd(raw_dir)
    } else {
        cat("Warning: temizhavaR.raw_dir option is not set. Using current directory.\n")
    }
    
    db_path <- normalizePath(db_path, mustWork = FALSE)
    cat("Database path:", db_path, "\n")
    
    mydb <- dbConnect(RSQLite::SQLite(), db_path)
    
    dbExecute(mydb, "CREATE TABLE IF NOT EXISTS location (
                        Istasyonlar_modified TEXT PRIMARY KEY,
                        station_type TEXT,
                        Sampling_Point_Id TEXT,
                        Longitude REAL,
                        Latitude REAL,
                        Altitude REAL,
                        LONGTD REAL,
                        LATTD REAL,
                        Air_Quality_Station_Area TEXT)")
    
    return(mydb)
}

normalize_text <- function(text) {
    text <- tolower(text)
    text <- gsub("[^a-z0-9çğıöşü ]", "", text)
    text <- gsub(" +", " ", text)
    text <- chartr("ğĞ", "gG", text)
    trimws(text)
}

find_best_match <- function(station_name, existing_stations, column_name) {
    if (nrow(existing_stations) == 0 || is.null(existing_stations[[column_name]])) return(NA)
    
    station_name_norm <- normalize_text(station_name)
    existing_stations_norm <- sapply(existing_stations[[column_name]], normalize_text)

    valid_indices <- !is.na(existing_stations_norm)
    existing_stations_norm <- existing_stations_norm[valid_indices]

    if (length(existing_stations_norm) == 0) return(NA)

    distances <- stringdist::stringdist(station_name_norm, existing_stations_norm, method = "jw")
    
    if (all(is.na(distances))) return(NA)

    min_dist <- min(distances, na.rm = TRUE)

    if (min_dist < 0.3) {
        return(existing_stations[[column_name]][valid_indices][which.min(distances)])
    } else {
        return(NA)
    }
}

station_exists <- function(station, station_list) {
    station_norm <- normalize_text(station)
    station_list_norm <- sapply(station_list, normalize_text)
    
    get_city <- function(x) {
        parts <- strsplit(x, "-")[[1]]
        return(normalize_text(parts[1]))
    }
    
    distances <- stringdist::stringdist(station_norm, station_list_norm, method = "jw")
    min_dist <- min(distances, na.rm = TRUE)
    
    if (min_dist < 0.4) {
        return(TRUE)
    }
    
    station_city <- get_city(station)
    station_list_cities <- sapply(station_list, get_city)
    
    city_distances <- stringdist::stringdist(station_city, station_list_cities, method = "jw")
    min_city_dist <- min(city_distances, na.rm = TRUE)
    
    return(min_city_dist < 0.3)
}

matchStationTypes <- function() {
    if (!is.null(sys.frame(1)$ofile)) {
        script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    } else {
        script_dir <- getwd()
    }
    station_file <- file.path(script_dir, "station_types.xlsx")
    if (!file.exists(station_file)) {
        stop("Station types file not found:", station_file)
    }
    station_types <- read_excel(station_file)
    db_path <- "../temiz-hava.sqlite"
    mydb <- initializeDatabase(db_path)
    tryCatch({
        dbExecute(mydb, "ALTER TABLE location ADD COLUMN PM10ISTASYON TEXT")
    }, error = function(e) {
        cat("Note: PM10ISTASYON column might already exist\n")
    })
    existing_stations <- dbGetQuery(mydb, "PRAGMA table_info(location)")
    column_names <- existing_stations$name
    if ("PM10ISTASYON" %in% column_names) {
        existing_stations <- dbGetQuery(mydb, "SELECT Istasyonlar_modified, PM10ISTASYON FROM location")
    } else {
        existing_stations <- dbGetQuery(mydb, "SELECT Istasyonlar_modified FROM location")
        existing_stations$PM10ISTASYON <- NA
    }
    matched_stations <- c()
    unmatched_stations <- c()
    for (i in seq_len(nrow(station_types))) {
        station_name <- station_types$`Air Quality Station Name`[i]
        best_match <- find_best_match(station_name, existing_stations, "Istasyonlar_modified")
        if (is.na(best_match) && "PM10ISTASYON" %in% column_names) {
            best_match <- find_best_match(station_name, existing_stations, "PM10ISTASYON")
        }
        if (!is.na(best_match)) {
            dbExecute(mydb, 
                     "UPDATE location SET 
                      station_type = ?, 
                      Sampling_Point_Id = ?, 
                      Longitude = ?, 
                      Latitude = ?, 
                      Altitude = ?, 
                      LONGTD = ?, 
                      LATTD = ?, 
                      Air_Quality_Station_Area = ?,
                      PM10ISTASYON = ?
                      WHERE lower(Istasyonlar_modified) = lower(?) 
                        OR (PM10ISTASYON IS NOT NULL AND lower(PM10ISTASYON) = lower(?))",
                     params = list(
                         station_types$`İstasyon türü`[i],
                         station_types$`Sampling Point Id`[i],
                         station_types$Longitude[i],
                         station_types$Latitude[i],
                         station_types$Altitude[i],
                         station_types$LONGTD[i],
                         station_types$LATTD[i],
                         station_types$`Air Quality Station Area`[i],
                         station_types$`Air Quality Station Name`[i],
                         best_match,
                         best_match
                     ))
            matched_stations <- c(matched_stations, station_name)
        } else {
            unmatched_stations <- c(unmatched_stations, station_name)
        }
    }
    sqlite_only <- character(0)
    excel_only <- character(0)
    na_station_types <- character(0)
    sqlite_stations <- dbGetQuery(mydb, "SELECT Istasyonlar_modified, station_type FROM location")
    excel_stations <- station_types$`Air Quality Station Name`
    sqlite_only <- sqlite_stations$Istasyonlar_modified[
        sapply(sqlite_stations$Istasyonlar_modified, 
               function(x) !station_exists(x, excel_stations))
    ]
    excel_only <- excel_stations[vapply(excel_stations, function(x) {
        !station_exists(x, sqlite_stations$Istasyonlar_modified)
    }, logical(1))]
    na_station_types <- sqlite_stations$Istasyonlar_modified[is.na(sqlite_stations$station_type)]
    cat("\n=== Matching Results ===\n")
    cat("Matched stations:", length(matched_stations), "\n")
    cat("Unmatched stations:", length(unmatched_stations), "\n")
    if (length(unmatched_stations) > 0) {
        cat("\nList of unmatched stations:\n")
        print(unmatched_stations)
    }
    cat("\n=== Additional Analysis ===\n")
    cat("\nStations in SQLite but not in Excel (", length(sqlite_only), "):\n")
    print(sqlite_only)
    cat("\nStations in Excel but not in SQLite (", length(excel_only), "):\n")
    print(excel_only)
    cat("\nStations with NA station_types (", length(na_station_types), "):\n")
    print(na_station_types)
    if (dbIsValid(mydb)) {
        dbDisconnect(mydb)
    }
}

matchStationTypes()
