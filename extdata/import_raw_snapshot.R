import_raw_snapshot <- function(
    years = c(2025L, 2024L),
    schema = "raw_20260908",
    result_dir = getOption("temizhavaR.base_dir"),
    validate_first = TRUE,
    station_keys = NULL,
    verbose = TRUE,
    scrape_date = Sys.Date()) {
  stopifnot(length(years) > 0L, all(!is.na(years)))
  years <- unique(as.integer(years))
  if (!grepl("^[a-z][a-z0-9_]*$", schema)) {
    stop("schema must contain only lower-case letters, numbers and underscores")
  }
  if (is.null(result_dir) || !nzchar(result_dir)) {
    stop("Set options(temizhavaR.base_dir=...) or pass result_dir")
  }
  scrape_date <- as.Date(scrape_date)
  if (length(scrape_date) != 1L || is.na(scrape_date)) {
    stop("scrape_date must be one valid date")
  }

  if (isTRUE(validate_first)) {
    validator <- file.path("extdata", "validate_raw_downloads.R")
    if (!file.exists(validator)) stop("Validator not found: ", validator)
    source(validator, local = environment())
    for (year in years) {
      validation <- validate_raw_downloads(
        year, result_dir = result_dir, check_content = TRUE
      )
      if (!isTRUE(validation$ok)) {
        stop("Raw files did not pass validation for ", year)
      }
    }
  }

  con <- temizhavaR:::create_postgres_conn()
  on.exit(temizhavaR:::disconnect_postgres(con), add = TRUE)
  schema_id <- DBI::dbQuoteIdentifier(con, schema)

  DBI::dbExecute(con, paste("CREATE SCHEMA IF NOT EXISTS", schema_id))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS ", schema_id, ".catalogue_snapshot (",
    "analysis_year integer NOT NULL, scrape_date date NOT NULL, ",
    "catalogue_status text NOT NULL, region text, city text NOT NULL, ",
    "station_original text NOT NULL, station_key text NOT NULL, ",
    "site_label text, ministry_station_id text, label_changed boolean, ",
    "checked_at text, PRIMARY KEY (analysis_year, station_key))"
  ))
  for (table in c("daily_detail", "hourly_detail")) {
    DBI::dbExecute(con, paste0(
      "CREATE TABLE IF NOT EXISTS ", schema_id, ".", table, " (",
      "id bigserial PRIMARY KEY, analysis_year integer NOT NULL, ",
      "scrape_date date NOT NULL, station_key text NOT NULL, city text NOT NULL, ",
      "observed_at_local timestamp without time zone NOT NULL, ",
      "observed_at_utc timestamp with time zone NOT NULL, ",
      "pm10 double precision, pm25 double precision, so2 double precision, ",
      "co double precision, no2 double precision, nox double precision, ",
      "no double precision, o3 double precision, source_file text NOT NULL, ",
      "UNIQUE (analysis_year, station_key, observed_at_local))"
    ))
    DBI::dbExecute(con, paste0(
      "CREATE INDEX IF NOT EXISTS ", table, "_year_station_idx ON ",
      schema_id, ".", table, " (analysis_year, station_key)"
    ))
  }
  DBI::dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS ", schema_id, ".no_data_status (",
    "analysis_year integer NOT NULL, scrape_date date NOT NULL, ",
    "station_key text NOT NULL, city text NOT NULL, data_type text NOT NULL, ",
    "status text NOT NULL, evidence text NOT NULL, marker_md5 text NOT NULL, ",
    "PRIMARY KEY (analysis_year, station_key, data_type))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS ", schema_id, ".import_manifest (",
    "analysis_year integer NOT NULL, data_type text NOT NULL, ",
    "station_key text NOT NULL, source_file text NOT NULL, source_md5 text NOT NULL, ",
    "row_count integer NOT NULL, imported_at timestamp with time zone NOT NULL DEFAULT now(), ",
    "PRIMARY KEY (analysis_year, data_type, station_key))"
  ))

  measurement_names <- c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3")

  normalize_header <- function(x) {
    x <- trimws(as.character(x))
    x <- sub("\\s*\\(.*$", "", x)
    x <- gsub("\\s+", "", x)
    x <- toupper(x)
    x <- gsub("PM2\\.?5", "PM25", x)
    x
  }

  parse_measurement <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "-", "NULL", "NA", "NaN", "*", "N/A")] <- NA_character_
    comma_decimal <- grepl(",", x, fixed = TRUE)
    x[comma_decimal] <- gsub("\\.", "", x[comma_decimal])
    x[comma_decimal] <- sub(",", ".", x[comma_decimal], fixed = TRUE)
    value <- suppressWarnings(as.numeric(x))
    value[!is.finite(value) | value <= -10000 | value >= 10000] <- NA_real_
    value
  }

  read_detail <- function(path, year, station_key, city) {
    raw <- readxl::read_excel(path, col_names = FALSE, .name_repair = "minimal")
    if (nrow(raw) < 3L || ncol(raw) < 2L) stop("No detail rows in ", path)
    header_1 <- as.character(unlist(raw[1, ], use.names = FALSE))
    header_2 <- as.character(unlist(raw[2, ], use.names = FALSE))
    header <- ifelse(!is.na(header_2) & nzchar(trimws(header_2)), header_2, header_1)
    header <- normalize_header(header)
    header[1] <- "TARIH"

    serial <- suppressWarnings(as.numeric(raw[-c(1, 2), 1][[1]]))
    rounded_serial <- round(serial * 24) / 24
    start_serial <- as.numeric(as.Date(sprintf("%04d-01-01", year)) - as.Date("1899-12-30"))
    end_serial <- as.numeric(as.Date(sprintf("%04d-01-01", year + 1L)) - as.Date("1899-12-30"))
    keep <- is.finite(rounded_serial) & rounded_serial >= start_serial & rounded_serial < end_serial
    if (!any(keep)) stop("No timestamps inside requested year in ", path)

    local_clock <- as.POSIXct(
      rounded_serial[keep] * 86400,
      origin = "1899-12-30", tz = "UTC"
    )
    local_text <- format(local_clock, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    utc_instant <- as.POSIXct(local_text, format = "%Y-%m-%d %H:%M:%S",
                             tz = "Europe/Istanbul")

    output <- data.frame(
      analysis_year = year,
      scrape_date = scrape_date,
      station_key = station_key,
      city = city,
      observed_at_local = local_clock,
      observed_at_utc = utc_instant,
      pm10 = NA_real_, pm25 = NA_real_, so2 = NA_real_, co = NA_real_,
      no2 = NA_real_, nox = NA_real_, no = NA_real_, o3 = NA_real_,
      source_file = basename(path), stringsAsFactors = FALSE
    )
    body <- raw[-c(1, 2), , drop = FALSE]
    for (parameter in measurement_names) {
      index <- which(header == parameter)
      if (length(index)) {
        output[[tolower(parameter)]] <- parse_measurement(body[[index[1]]][keep])
      }
    }
    output <- output[!duplicated(output$observed_at_local), , drop = FALSE]
    output
  }

  append_in_chunks <- function(table, data, chunk_size = 10000L) {
    starts <- seq.int(1L, nrow(data), by = chunk_size)
    for (start in starts) {
      end <- min(start + chunk_size - 1L, nrow(data))
      DBI::dbWriteTable(
        con, DBI::Id(schema = schema, table = table),
        data[start:end, , drop = FALSE], append = TRUE, copy = TRUE
      )
    }
  }

  imported_files <- 0L
  imported_rows <- 0L
  skipped_files <- 0L

  for (year in years) {
    catalogue_path <- file.path(result_dir, paste0("station_catalogue_", year, ".csv"))
    catalogue <- read.csv(catalogue_path, stringsAsFactors = FALSE, check.names = FALSE)
    catalogue_rows <- data.frame(
      analysis_year = year, scrape_date = scrape_date,
      catalogue_status = catalogue$status, region = catalogue$Bolge,
      city = catalogue$Sehir, station_original = catalogue$Istasyon_original,
      station_key = catalogue$file_key, site_label = catalogue$site_label,
      ministry_station_id = catalogue$site_station_id,
      label_changed = catalogue$label_changed, checked_at = catalogue$checked_at,
      stringsAsFactors = FALSE
    )
    existing_catalogue <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*)::int AS n FROM ", schema_id,
             ".catalogue_snapshot WHERE analysis_year = $1"),
      params = list(year)
    )$n[[1]]
    if (existing_catalogue == 0L) {
      DBI::dbAppendTable(
        con, DBI::Id(schema = schema, table = "catalogue_snapshot"),
        catalogue_rows
      )
    } else if (existing_catalogue != nrow(catalogue_rows)) {
      stop("Existing catalogue snapshot has an unexpected row count for ", year)
    }

    validation <- read.csv(
      file.path(result_dir, paste0("raw_download_validation_", year, ".csv")),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    if (any(validation$result == "incomplete_or_invalid")) {
      stop("Validation report still has incomplete rows for ", year)
    }

    status_rows <- validation[validation$result != "data_files_valid", , drop = FALSE]
    if (!is.null(station_keys)) {
      status_rows <- status_rows[status_rows$file_key %in% station_keys, , drop = FALSE]
    }
    for (i in seq_len(nrow(status_rows))) {
      type_key <- if (status_rows$data_type[i] == "daily") "gunluk" else "saatlik"
      marker <- file.path(
        result_dir, status_rows$city[i],
        paste0(status_rows$file_key[i], "_", type_key, "_no_data_",
               year, "-", year + 1L, ".txt")
      )
      evidence <- paste(readLines(marker, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      status_data <- data.frame(
        analysis_year = year, scrape_date = scrape_date,
        station_key = status_rows$file_key[i], city = status_rows$city[i],
        data_type = status_rows$data_type[i], status = status_rows$result[i],
        evidence = evidence, marker_md5 = unname(tools::md5sum(marker)),
        stringsAsFactors = FALSE
      )
      exists <- DBI::dbGetQuery(
        con,
        paste0("SELECT COUNT(*)::int AS n FROM ", schema_id,
               ".no_data_status WHERE analysis_year = $1 AND station_key = $2 AND data_type = $3"),
        params = list(year, status_rows$file_key[i], status_rows$data_type[i])
      )$n[[1]]
      if (exists == 0L) {
        DBI::dbAppendTable(
          con, DBI::Id(schema = schema, table = "no_data_status"), status_data
        )
      }
    }

    data_rows <- validation[validation$result == "data_files_valid", , drop = FALSE]
    if (!is.null(station_keys)) {
      data_rows <- data_rows[data_rows$file_key %in% station_keys, , drop = FALSE]
    }
    for (i in seq_len(nrow(data_rows))) {
      data_type <- data_rows$data_type[i]
      type_key <- if (data_type == "daily") "gunluk" else "saatlik"
      table <- paste0(data_type, "_detail")
      path <- file.path(
        result_dir, data_rows$city[i],
        paste0(data_rows$file_key[i], "_", type_key, "_detay_",
               year, "-", year + 1L, ".xlsx")
      )
      md5 <- unname(tools::md5sum(path))
      manifest <- DBI::dbGetQuery(
        con,
        paste0("SELECT source_md5, row_count FROM ", schema_id,
               ".import_manifest WHERE analysis_year = $1 AND data_type = $2 AND station_key = $3"),
        params = list(year, data_type, data_rows$file_key[i])
      )
      if (nrow(manifest) == 1L && identical(manifest$source_md5[[1]], md5)) {
        skipped_files <- skipped_files + 1L
        next
      }

      parsed <- read_detail(path, year, data_rows$file_key[i], data_rows$city[i])
      DBI::dbWithTransaction(con, {
        if (nrow(manifest)) {
          DBI::dbExecute(
            con,
            paste0("DELETE FROM ", schema_id, ".", table,
                   " WHERE analysis_year = $1 AND station_key = $2"),
            params = list(year, data_rows$file_key[i])
          )
          DBI::dbExecute(
            con,
            paste0("DELETE FROM ", schema_id,
                   ".import_manifest WHERE analysis_year = $1 AND data_type = $2 AND station_key = $3"),
            params = list(year, data_type, data_rows$file_key[i])
          )
        }
        append_in_chunks(table, parsed)
        DBI::dbAppendTable(
          con, DBI::Id(schema = schema, table = "import_manifest"),
          data.frame(
            analysis_year = year, data_type = data_type,
            station_key = data_rows$file_key[i], source_file = basename(path),
            source_md5 = md5, row_count = nrow(parsed),
            stringsAsFactors = FALSE
          )
        )
      })
      imported_files <- imported_files + 1L
      imported_rows <- imported_rows + nrow(parsed)
      if (isTRUE(verbose) && imported_files %% 25L == 0L) {
        cat("Imported", imported_files, "files and", imported_rows, "rows\n")
      }
    }
  }

  result <- list(
    schema = schema, imported_files = imported_files,
    imported_rows = imported_rows, skipped_files = skipped_files
  )
  if (isTRUE(verbose)) print(result)
  invisible(result)
}
