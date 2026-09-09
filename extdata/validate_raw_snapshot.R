validate_raw_snapshot <- function(
    years = c(2025L, 2024L),
    schema = "raw_20260908",
    result_dir = getOption("temizhavaR.base_dir")) {
  years <- unique(as.integer(years))
  if (!grepl("^[a-z][a-z0-9_]*$", schema)) stop("Invalid schema name")
  if (is.null(result_dir) || !nzchar(result_dir)) stop("result_dir is required")

  con <- temizhavaR:::create_postgres_conn()
  on.exit(temizhavaR:::disconnect_postgres(con), add = TRUE)
  schema_id <- DBI::dbQuoteIdentifier(con, schema)
  checks <- list()
  add_check <- function(year, check, expected, actual) {
    checks[[length(checks) + 1L]] <<- data.frame(
      year = year, check = check, expected = as.character(expected),
      actual = as.character(actual), passed = identical(as.character(expected), as.character(actual)),
      stringsAsFactors = FALSE
    )
  }

  for (year in years) {
    local_validation <- read.csv(
      file.path(result_dir, paste0("raw_download_validation_", year, ".csv")),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    expected_data <- local_validation[local_validation$result == "data_files_valid", ]
    expected_status <- local_validation[local_validation$result != "data_files_valid", ]
    expected_catalogue <- nrow(read.csv(
      file.path(result_dir, paste0("station_catalogue_", year, ".csv")),
      stringsAsFactors = FALSE, check.names = FALSE
    ))

    catalogue_count <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*)::int AS n FROM ", schema_id,
             ".catalogue_snapshot WHERE analysis_year = $1"),
      params = list(year)
    )$n[[1]]
    add_check(year, "catalogue rows", expected_catalogue, catalogue_count)

    status_count <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*)::int AS n FROM ", schema_id,
             ".no_data_status WHERE analysis_year = $1"),
      params = list(year)
    )$n[[1]]
    add_check(year, "no-data/catalogue-status rows", nrow(expected_status), status_count)

    for (data_type in c("daily", "hourly")) {
      table <- paste0(data_type, "_detail")
      expected_files <- sum(expected_data$data_type == data_type)
      manifest <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*)::int AS files, COALESCE(SUM(row_count), 0)::bigint AS rows ",
          "FROM ", schema_id,
          ".import_manifest WHERE analysis_year = $1 AND data_type = $2"
        ),
        params = list(year, data_type)
      )
      actual <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*)::bigint AS rows, COUNT(DISTINCT station_key)::int AS stations, ",
          "COUNT(*) - COUNT(DISTINCT (station_key, observed_at_local)) AS duplicates, ",
          "COUNT(*) FILTER (WHERE EXTRACT(MINUTE FROM observed_at_local) <> 0 ",
          "OR EXTRACT(SECOND FROM observed_at_local) <> 0)::bigint AS off_clock, ",
          "COUNT(*) FILTER (WHERE observed_at_local < make_date($1, 1, 1) ",
          "OR observed_at_local >= make_date($1 + 1, 1, 1))::bigint AS outside_year ",
          "FROM ", schema_id, ".", table, " WHERE analysis_year = $1"
        ),
        params = list(year)
      )
      manifest_mismatch <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*)::int AS n FROM (",
          "SELECT m.station_key, m.row_count, COUNT(d.id)::int AS actual_rows ",
          "FROM ", schema_id, ".import_manifest m LEFT JOIN ", schema_id, ".", table,
          " d ON d.analysis_year = m.analysis_year AND d.station_key = m.station_key ",
          "WHERE m.analysis_year = $1 AND m.data_type = $2 ",
          "GROUP BY m.station_key, m.row_count HAVING m.row_count <> COUNT(d.id)) x"
        ),
        params = list(year, data_type)
      )$n[[1]]

      add_check(year, paste(data_type, "manifest files"), expected_files, manifest$files[[1]])
      add_check(year, paste(data_type, "table rows"), manifest$rows[[1]], actual$rows[[1]])
      add_check(year, paste(data_type, "stations"), expected_files, actual$stations[[1]])
      add_check(year, paste(data_type, "duplicate station-times"), 0, actual$duplicates[[1]])
      add_check(year, paste(data_type, "off-clock timestamps"), 0, actual$off_clock[[1]])
      add_check(year, paste(data_type, "timestamps outside year"), 0, actual$outside_year[[1]])
      add_check(year, paste(data_type, "manifest row mismatches"), 0, manifest_mismatch)
    }
  }

  report <- do.call(rbind, checks)
  report_path <- file.path(result_dir, paste0("raw_snapshot_validation_", schema, ".csv"))
  write.csv(report, report_path, row.names = FALSE, fileEncoding = "UTF-8")
  print(report, row.names = FALSE)
  cat("Report:", report_path, "\n")
  invisible(list(ok = all(report$passed), report = report, report_path = report_path))
}
