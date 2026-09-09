validate_raw_downloads <- function(year,
                                   result_dir = getOption("temizhavaR.base_dir"),
                                   check_content = FALSE) {
  if (length(year) != 1 || is.na(year)) stop("'year' must be one year")
  if (is.null(result_dir) || !nzchar(result_dir)) {
    stop("Set options(temizhavaR.base_dir=...) or pass result_dir")
  }

  catalogue_file <- file.path(result_dir, paste0("station_catalogue_", year, ".csv"))
  if (!file.exists(catalogue_file)) {
    stop("Catalogue report does not exist: ", catalogue_file)
  }
  catalogue <- read.csv(
    catalogue_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
    check.names = FALSE
  )
  required_columns <- c("status", "Sehir", "Istasyon_original", "file_key")
  if (!all(required_columns %in% names(catalogue))) {
    stop("Catalogue report is missing required columns")
  }

  xlsx_is_readable <- function(path) {
    if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
      return(FALSE)
    }
    tryCatch({
      members <- utils::unzip(path, list = TRUE)$Name
      any(grepl("^xl/worksheets/sheet[0-9]+\\.xml$", members)) &&
        "[Content_Types].xml" %in% members
    }, error = function(e) FALSE)
  }

  detail_content_check <- function(path) {
    empty_result <- list(
      valid = FALSE, date_count = 0L, first_date = NA_character_,
      last_date = NA_character_
    )
    if (!xlsx_is_readable(path)) return(empty_result)

    tryCatch({
      first_column <- openxlsx::read.xlsx(
        path, sheet = 1, cols = 1, colNames = FALSE,
        skipEmptyRows = TRUE, check.names = FALSE
      )
      if (ncol(first_column) != 1L) return(empty_result)

      excel_dates <- suppressWarnings(as.numeric(first_column[[1]]))
      excel_dates <- excel_dates[is.finite(excel_dates)]
      if (!length(excel_dates)) return(empty_result)

      start_serial <- as.numeric(
        as.Date(sprintf("%04d-01-01", as.integer(year))) - as.Date("1899-12-30")
      )
      end_serial <- as.numeric(
        as.Date(sprintf("%04d-01-01", as.integer(year) + 1L)) - as.Date("1899-12-30")
      )
      dates <- as.Date(floor(excel_dates), origin = "1899-12-30")
      list(
        valid = all(excel_dates >= start_serial & excel_dates < end_serial + 1) &&
          all(diff(excel_dates) >= 0),
        date_count = length(excel_dates),
        first_date = format(min(dates), "%Y-%m-%d"),
        last_date = format(max(dates), "%Y-%m-%d")
      )
    }, error = function(e) empty_result)
  }

  marker_is_valid <- function(path, status, station, site_id, data_type) {
    if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
      return(FALSE)
    }
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character())
    if (!length(lines) ||
        !any(lines == paste0("Station: ", station)) ||
        !any(lines == paste0("Data type: ", data_type))) {
      return(FALSE)
    }

    if (identical(status, "not_in_current_site")) {
      return(any(lines == paste0(
        "Period: 01.01.", year, " to 01.01.", as.integer(year) + 1L
      )) && any(lines == paste0(
        "Status: Station is not present in the ministry's current download catalogue."
      )))
    }

    expected_period <- if (identical(data_type, "hourly")) {
      paste0(
        "Submitted period: 01.01.", year, " 00:00 to 01.01.",
        as.integer(year) + 1L, " 00:00"
      )
    } else {
      paste0(
        "Submitted period: 01.01.", year, " to 01.01.",
        as.integer(year) + 1L
      )
    }
    id_ok <- !is.na(site_id) && nzchar(site_id) &&
      any(lines == paste0("Ministry station ID: ", site_id))
    parameter_ok <- any(grepl(
      "^Selected parameter count: [1-9][0-9]*$", lines
    ))
    id_ok && parameter_ok && any(lines == expected_period) &&
      any(lines == "Status: The ministry report returned Result=false (no data).")
  }

  period <- paste0(year, "-", year + 1L)
  rows <- vector("list", nrow(catalogue) * 2L)
  row_index <- 0L
  for (i in seq_len(nrow(catalogue))) {
    for (data_type in c("daily", "hourly")) {
      row_index <- row_index + 1L
      type_key <- if (data_type == "daily") "gunluk" else "saatlik"
      detail <- file.path(
        result_dir, catalogue$Sehir[i],
        paste0(catalogue$file_key[i], "_", type_key, "_detay_", period, ".xlsx")
      )
      summary <- file.path(
        result_dir, catalogue$Sehir[i],
        paste0(catalogue$file_key[i], "_", type_key, "_ozet_", period, ".xlsx")
      )
      marker <- file.path(
        result_dir, catalogue$Sehir[i],
        paste0(catalogue$file_key[i], "_", type_key, "_no_data_", period, ".txt")
      )
      detail_ok <- xlsx_is_readable(detail)
      summary_ok <- xlsx_is_readable(summary)
      content <- if (isTRUE(check_content) && detail_ok) {
        detail_content_check(detail)
      } else {
        list(
          valid = if (detail_ok) NA else FALSE,
          date_count = NA_integer_, first_date = NA_character_,
          last_date = NA_character_
        )
      }
      detail_complete <- detail_ok &&
        (!isTRUE(check_content) || isTRUE(content$valid))
      site_id <- if ("site_station_id" %in% names(catalogue)) {
        catalogue$site_station_id[i]
      } else {
        NA_character_
      }
      marker_ok <- marker_is_valid(
        marker, catalogue$status[i], catalogue$Istasyon_original[i],
        site_id, data_type
      )
      result <- if (detail_complete && summary_ok) {
        "data_files_valid"
      } else if (!file.exists(detail) && !file.exists(summary) && marker_ok) {
        if (catalogue$status[i] == "not_in_current_site") {
          "not_in_current_catalogue"
        } else {
          "ministry_reported_no_data"
        }
      } else {
        "incomplete_or_invalid"
      }
      rows[[row_index]] <- data.frame(
        year = as.integer(year),
        catalogue_status = catalogue$status[i],
        city = catalogue$Sehir[i],
        station = catalogue$Istasyon_original[i],
        file_key = catalogue$file_key[i],
        data_type = data_type,
        result = result,
        detail_exists = file.exists(detail),
        detail_valid = detail_ok,
        detail_content_valid = content$valid,
        date_count = content$date_count,
        first_date = content$first_date,
        last_date = content$last_date,
        summary_exists = file.exists(summary),
        summary_valid = summary_ok,
        marker_exists = marker_ok,
        stringsAsFactors = FALSE
      )
    }
  }
  report <- do.call(rbind, rows)
  report_file <- file.path(result_dir, paste0("raw_download_validation_", year, ".csv"))
  write.csv(report, report_file, row.names = FALSE, fileEncoding = "UTF-8")
  counts <- sort(table(report$result), decreasing = TRUE)
  cat("Raw download validation:", year, "\n")
  for (name in names(counts)) cat("-", name, ":", counts[[name]], "\n")
  cat("Report:", report_file, "\n")
  ok <- !any(report$result == "incomplete_or_invalid")
  invisible(list(ok = ok, report = report, report_file = report_file))
}
