library(readxl)
library(DBI)
library(stringdist)
library(temizhavaR)

initializeDatabase <- function() {
    base_dir <- getOption("temizhavaR.base_dir")
    
    if (!is.null(base_dir)) {
        setwd(base_dir)
    } else {
        cat("Warning: temizhavaR.base_dir option is not set. Using current directory.\n")
    }
    
    mydb <- create_postgres_conn()
    
    dbExecute(mydb, "CREATE TABLE IF NOT EXISTS location (
                        \"Istasyon_modified\" TEXT PRIMARY KEY,
                        \"station_type\" TEXT,
                        \"Sampling_Point_Id\" TEXT,
                        \"Longitude\" REAL,
                        \"Latitude\" REAL,
                        \"Altitude\" REAL,
                        \"LONGTD\" REAL,
                        \"LATTD\" REAL,
                        \"Air_Quality_Station_Area\" TEXT,
                        \"PM10ISTASYON\" TEXT)")
                        
    
    return(mydb)
}

normalize_text <- function(text) {
    text <- tolower(text)
    text <- gsub("[^a-z0-9çğıöşü ]", "", text)
    text <- gsub(" +", " ", text)
    text <- chartr("ğĞ", "gG", text)
    text <- chartr("ıİ", "iI", text)
    text <- chartr("öÖ", "oO", text)
    text <- chartr("şŞ", "sS", text)
    text <- chartr("üÜ", "uU", text)
    text <- chartr("çÇ", "cC", text)
    return(
         trimws(text)
    )
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
    mydb <- initializeDatabase()
    tryCatch({
        dbExecute(mydb, "ALTER TABLE location ADD COLUMN station_type TEXT")
    }, error = function(e) {
        cat("Note: station_type column might already exist\n")
    })
    tryCatch({
        dbExecute(mydb, "ALTER TABLE location ADD COLUMN PM10ISTASYON TEXT")
    }, error = function(e) {
        cat("Note: PM10ISTASYON column might already exist\n")
    })
    existing_stations <- dbGetQuery(mydb, "
        SELECT column_name 
        FROM information_schema.columns 
        WHERE table_name = 'location'")
    column_names <- existing_stations$column_name
    if ("pm10istasyon" %in% tolower(column_names)) {
        existing_stations <- dbGetQuery(mydb, 'SELECT "Istasyon_modified", "PM10ISTASYON" FROM location')
    } else {
        existing_stations <- dbGetQuery(mydb, 'SELECT "Istasyon_modified" FROM location')
        existing_stations$PM10ISTASYON <- NA
    }
    matched_stations <- c()
    unmatched_stations <- c()
    for (i in seq_len(nrow(station_types))) {
        station_name <- station_types$`Air Quality Station Name`[i]
        best_match <- find_best_match(station_name, existing_stations, "Istasyon_modified")
        if (is.na(best_match) && "PM10ISTASYON" %in% column_names) {
            best_match <- find_best_match(station_name, existing_stations, "PM10ISTASYON")
        }
        if (!is.na(best_match)) {
            dbExecute(mydb, 
                     'UPDATE location SET 
                      "station_type" = $1, 
                      "Sampling_Point_Id" = $2, 
                      "Longitude" = $3, 
                      "Latitude" = $4, 
                      "Altitude" = $5, 
                      "LONGTD" = $6, 
                      "LATTD" = $7, 
                      "Air_Quality_Station_Area" = $8,
                      "PM10ISTASYON" = $9
                      WHERE LOWER("Istasyon_modified") = LOWER($10) 
                        OR ("PM10ISTASYON" IS NOT NULL AND LOWER("PM10ISTASYON") = LOWER($10))',
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
                         best_match
                     ))
            matched_stations <- c(matched_stations, station_name)
        } else {
            unmatched_stations <- c(unmatched_stations, station_name)
        }
    }

    turkey_csv_file <- file.path(script_dir, "Turkey_Stations.csv")
    if (file.exists(turkey_csv_file) && length(unmatched_stations) > 0) {
        turkey_stations <- read.csv(turkey_csv_file, stringsAsFactors = FALSE)
        for (station_name in unmatched_stations) {
            best_match_csv <- find_best_match(station_name, turkey_stations, "Air Quality Station Name")
            if (!is.na(best_match_csv)) {
                station_type_csv <- turkey_stations[turkey_stations$`Air Quality Station Name` == best_match_csv, 
                                                    "Air Quality Station Type"]
                if (!is.na(station_type_csv[1])) {
                    dbExecute(mydb,
                        'UPDATE location SET "station_type" = $1 
                         WHERE lower("Istasyon_modified") = lower($2)',
                        params = list(station_type_csv[1], best_match_csv))
                    matched_stations <- c(matched_stations, station_name)
                }
            }
        }
        unmatched_stations <- setdiff(unmatched_stations, matched_stations)
    }

    postgres_only <- character(0)
    excel_only <- character(0)
    na_station_types <- character(0)
    
    postgres_stations <- dbGetQuery(mydb, 'SELECT "Istasyon_modified", "station_type" FROM location')
    excel_stations <- station_types$`Air Quality Station Name`
    
    postgres_stations$Istasyon_modified <- as.character(postgres_stations$Istasyon_modified)
    postgres_stations$station_type <- as.character(postgres_stations$station_type)
    
    postgres_only <- postgres_stations$Istasyon_modified[
        !sapply(postgres_stations$Istasyon_modified, function(x) {
            any(sapply(excel_stations, function(y) station_exists(x, y)))
        })
    ]
    
    excel_only <- excel_stations[
        !sapply(excel_stations, function(x) {
            any(sapply(postgres_stations$Istasyon_modified, function(y) station_exists(x, y)))
        })
    ]
    
    na_station_types <- postgres_stations$Istasyon_modified[
        is.na(postgres_stations$station_type)
    ]

    cat("\n=== Matching Results ===\n")
    cat("Matched stations:", length(matched_stations), "\n")
    cat("Unmatched stations:", length(unmatched_stations), "\n")
    if (length(unmatched_stations) > 0) {
        cat("\nList of unmatched stations:\n")
        print(unmatched_stations)
    }
    cat("\n=== Additional Analysis ===\n")
    cat("\nStations in postgresbut not in Excel (", length(postgres_only), "):\n")
    print(postgres_only)
    cat("\nStations in Excel but not in postgres (", length(excel_only), "):\n")
    print(excel_only)
    cat("\nStations with NA station_types (", length(na_station_types), "):\n")
    print(na_station_types)
    if (dbIsValid(mydb)) {
        dbDisconnect(mydb)
    }
}

matchStationTypes()
