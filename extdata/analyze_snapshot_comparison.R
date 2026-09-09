analyze_snapshot_comparison <- function(
    schema = "raw_20260908",
    result_dir = getOption("temizhavaR.base_dir"),
    thresholds = c(75, 80, 85, 89.5, 90, 95)) {
  if (!grepl("^[a-z][a-z0-9_]*$", schema)) stop("Invalid schema name")
  if (is.null(result_dir) || !nzchar(result_dir)) stop("result_dir is required")
  parameters <- c("pm10", "pm25", "so2", "co", "no2", "nox", "no", "o3")
  con <- temizhavaR:::create_postgres_conn()
  on.exit(temizhavaR:::disconnect_postgres(con), add = TRUE)
  schema_id <- DBI::dbQuoteIdentifier(con, schema)

  comparison_parts <- list()
  for (data_type in c("daily", "hourly")) {
    table <- paste0(data_type, "_detail")
    old_columns <- paste(
      sprintf('"%s" AS %s', toupper(parameters), parameters), collapse = ", "
    )
    new_columns <- paste(parameters, collapse = ", ")
    metric_sql <- unlist(lapply(parameters, function(parameter) c(
      sprintf("COUNT(*) FILTER (WHERE old_%s IS NOT NULL)::int AS %s_old_values", parameter, parameter),
      sprintf("COUNT(*) FILTER (WHERE new_%s IS NOT NULL)::int AS %s_new_values", parameter, parameter),
      sprintf("COUNT(*) FILTER (WHERE old_time IS NOT NULL AND new_time IS NOT NULL AND old_%s IS NULL AND new_%s IS NOT NULL)::int AS %s_gained_values", parameter, parameter, parameter),
      sprintf("COUNT(*) FILTER (WHERE old_time IS NOT NULL AND new_time IS NOT NULL AND old_%s IS NOT NULL AND new_%s IS NULL)::int AS %s_lost_values", parameter, parameter, parameter),
      sprintf("COUNT(*) FILTER (WHERE old_%s IS NOT NULL AND new_%s IS NOT NULL AND ABS(old_%s - new_%s) > 1e-9)::int AS %s_changed_values", parameter, parameter, parameter, parameter, parameter)
    )))
    joined_columns <- paste(
      unlist(lapply(parameters, function(parameter) c(
        sprintf("o.%s AS old_%s", parameter, parameter),
        sprintf("n.%s AS new_%s", parameter, parameter)
      ))), collapse = ", "
    )
    query <- paste0(
      "WITH old_data AS (SELECT \"Istasyon_modified\" AS station_key, ",
      "date_trunc('hour', \"Tarih_NOTZ\" + interval '30 minutes') AS observed_at_local, ",
      old_columns, " FROM public.", table,
      " WHERE \"Tarih_NOTZ\" >= timestamp '2024-01-01' ",
      "AND \"Tarih_NOTZ\" < timestamp '2025-01-01'), ",
      "new_data AS (SELECT station_key, observed_at_local, ", new_columns,
      " FROM ", schema_id, ".", table, " WHERE analysis_year = 2024), ",
      "joined AS (SELECT COALESCE(o.station_key, n.station_key) AS station_key, ",
      "o.observed_at_local AS old_time, n.observed_at_local AS new_time, ",
      joined_columns, " FROM old_data o FULL OUTER JOIN new_data n ",
      "USING (station_key, observed_at_local)) ",
      "SELECT '", data_type, "'::text AS data_type, station_key, ",
      "COUNT(*) FILTER (WHERE old_time IS NOT NULL)::int AS old_rows, ",
      "COUNT(*) FILTER (WHERE new_time IS NOT NULL)::int AS new_rows, ",
      "COUNT(*) FILTER (WHERE old_time IS NOT NULL AND new_time IS NOT NULL)::int AS common_rows, ",
      "COUNT(*) FILTER (WHERE old_time IS NOT NULL AND new_time IS NULL)::int AS only_old_rows, ",
      "COUNT(*) FILTER (WHERE old_time IS NULL AND new_time IS NOT NULL)::int AS only_new_rows, ",
      paste(metric_sql, collapse = ", "),
      " FROM joined GROUP BY station_key ORDER BY station_key"
    )
    comparison_parts[[data_type]] <- DBI::dbGetQuery(con, query)
  }
  comparison <- do.call(rbind, comparison_parts)
  comparison_path <- file.path(result_dir, "scrape_comparison_2024_station.csv")
  write.csv(comparison, comparison_path, row.names = FALSE, fileEncoding = "UTF-8")
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "comparison_2024_station"),
    comparison, overwrite = TRUE, copy = TRUE
  )

  count_query <- function(table, source, year) {
    expected <- if (table == "daily_detail") {
      if (year == 2024L) 366L else 365L
    } else {
      if (year == 2024L) 8784L else 8760L
    }
    if (source == "new_scrape") {
      universe <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT station_key, catalogue_status FROM ", schema_id,
          ".catalogue_snapshot WHERE analysis_year = $1 ",
          "AND catalogue_status <> 'not_in_current_site' ORDER BY station_key"
        ),
        params = list(year)
      )
      sql <- paste0(
        "SELECT station_key, ",
        paste(sprintf("COUNT(%s)::int AS %s", parameters, parameters), collapse = ", "),
        " FROM ", schema_id, ".", table,
        " WHERE analysis_year = $1 GROUP BY station_key"
      )
      counts <- DBI::dbGetQuery(con, sql, params = list(year))
    } else {
      universe <- DBI::dbGetQuery(
        con,
        "SELECT \"Istasyon_modified\" AS station_key, 'legacy_location'::text AS catalogue_status FROM public.location ORDER BY 1"
      )
      sql <- paste0(
        "SELECT \"Istasyon_modified\" AS station_key, ",
        paste(sprintf('COUNT("%s")::int AS %s', toupper(parameters), parameters), collapse = ", "),
        " FROM public.", table,
        " WHERE \"Tarih_NOTZ\" >= timestamp '2024-01-01' ",
        "AND \"Tarih_NOTZ\" < timestamp '2025-01-01' GROUP BY 1"
      )
      counts <- DBI::dbGetQuery(con, sql)
    }
    merged <- merge(universe, counts, by = "station_key", all.x = TRUE)
    merged[parameters] <- lapply(merged[parameters], function(x) {
      x[is.na(x)] <- 0L
      as.integer(x)
    })
    detail <- do.call(rbind, lapply(parameters, function(parameter) {
      data.frame(
        source = source, year = year,
        data_type = sub("_detail$", "", table),
        station_key = merged$station_key,
        catalogue_status = merged$catalogue_status,
        parameter = toupper(parameter), valid_count = merged[[parameter]],
        expected_count = expected,
        availability_pct = pmin(100, merged[[parameter]] / expected * 100),
        stringsAsFactors = FALSE
      )
    }))
    detail
  }

  availability <- do.call(rbind, c(
    lapply(c(2024L, 2025L), function(year) {
      do.call(rbind, lapply(c("daily_detail", "hourly_detail"), function(table) {
        count_query(table, "new_scrape", year)
      }))
    }),
    lapply(c("daily_detail", "hourly_detail"), function(table) {
      count_query(table, "legacy_public", 2024L)
    })
  ))
  availability$availability_pct <- round(availability$availability_pct, 4)
  availability_path <- file.path(result_dir, "data_availability_station_thresholds.csv")
  write.csv(availability, availability_path, row.names = FALSE, fileEncoding = "UTF-8")

  group_keys <- c("source", "year", "data_type", "parameter")
  groups <- split(availability, interaction(availability[group_keys], drop = TRUE))
  threshold_summary <- do.call(rbind, lapply(groups, function(group) {
    do.call(rbind, lapply(thresholds, function(threshold) {
      data.frame(
        source = group$source[1], year = group$year[1],
        data_type = group$data_type[1], parameter = group$parameter[1],
        threshold = threshold, eligible_stations = nrow(group),
        minimum_valid_required = ceiling(group$expected_count[1] * threshold / 100),
        stations_meeting = sum(group$availability_pct >= threshold),
        station_coverage_pct = round(
          sum(group$availability_pct >= threshold) / nrow(group) * 100, 2
        ), stringsAsFactors = FALSE
      )
    }))
  }))
  rownames(threshold_summary) <- NULL
  threshold_summary <- threshold_summary[order(
    threshold_summary$source, threshold_summary$year,
    threshold_summary$data_type, threshold_summary$parameter,
    threshold_summary$threshold
  ), ]
  threshold_path <- file.path(result_dir, "data_availability_threshold_summary.csv")
  write.csv(threshold_summary, threshold_path, row.names = FALSE, fileEncoding = "UTF-8")
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "availability_station"),
    availability, overwrite = TRUE, copy = TRUE
  )
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "threshold_summary"),
    threshold_summary, overwrite = TRUE, copy = TRUE
  )

  threshold_delta <- merge(
    threshold_summary[threshold_summary$threshold == 89.5, ],
    threshold_summary[threshold_summary$threshold == 90, ],
    by = group_keys, suffixes = c("_89_5", "_90")
  )
  threshold_delta$additional_stations_at_89_5 <-
    threshold_delta$stations_meeting_89_5 - threshold_delta$stations_meeting_90
  delta_path <- file.path(result_dir, "data_availability_89_5_vs_90.csv")
  write.csv(threshold_delta, delta_path, row.names = FALSE, fileEncoding = "UTF-8")

  threshold_band <- availability[
    availability$availability_pct >= 89.5 & availability$availability_pct < 90,
    c("source", "year", "data_type", "station_key", "parameter",
      "valid_count", "expected_count", "availability_pct")
  ]
  threshold_band <- threshold_band[order(
    threshold_band$source, threshold_band$year, threshold_band$data_type,
    threshold_band$parameter, threshold_band$station_key
  ), ]
  band_path <- file.path(result_dir, "data_availability_89_5_band_stations.csv")
  write.csv(threshold_band, band_path, row.names = FALSE, fileEncoding = "UTF-8")
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "threshold_89_5_band_stations"),
    threshold_band, overwrite = TRUE, copy = TRUE
  )

  comparison_parameter <- do.call(rbind, lapply(parameters, function(parameter) {
    part <- aggregate(
      comparison[c(
        paste0(parameter, "_old_values"), paste0(parameter, "_new_values"),
        paste0(parameter, "_gained_values"), paste0(parameter, "_lost_values"),
        paste0(parameter, "_changed_values")
      )],
      list(data_type = comparison$data_type), sum
    )
    names(part)[2:6] <- c(
      "old_values", "new_values", "gained_values", "lost_values", "changed_values"
    )
    part$parameter <- toupper(parameter)
    part[c("data_type", "parameter", "old_values", "new_values", "gained_values",
           "lost_values", "changed_values")]
  }))
  parameter_path <- file.path(result_dir, "scrape_comparison_2024_parameter_summary.csv")
  write.csv(comparison_parameter, parameter_path, row.names = FALSE, fileEncoding = "UTF-8")
  DBI::dbWriteTable(
    con, DBI::Id(schema = schema, table = "comparison_2024_parameter_summary"),
    comparison_parameter, overwrite = TRUE, copy = TRUE
  )

  comparison_summary <- aggregate(
    comparison[c("old_rows", "new_rows", "common_rows", "only_old_rows", "only_new_rows")],
    list(data_type = comparison$data_type), sum
  )
  report_path <- file.path(result_dir, "scrape_and_threshold_summary.md")
  lines <- c(
    "# 2024 eski–yeni scrape ve doluluk eşiği özeti",
    "",
    paste0("Üretim zamanı: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    "",
    "Eski `public` tablolar değiştirilmemiştir. Yeni scrape `raw_20260908` şemasındadır.",
    "Doluluk paydaları 2024 için 366 gün/8.784 saat, 2025 için 365 gün/8.760 saattir.",
    "",
    "## 2024 satır karşılaştırması",
    ""
  )
  for (i in seq_len(nrow(comparison_summary))) {
    x <- comparison_summary[i, ]
    lines <- c(lines, sprintf(
      "- %s: eski %s, yeni %s, ortak %s, yalnız eski %s, yalnız yeni %s",
      x$data_type, x$old_rows, x$new_rows, x$common_rows,
      x$only_old_rows, x$only_new_rows
    ))
  }
  station_set <- do.call(rbind, lapply(split(comparison, comparison$data_type), function(x) {
    data.frame(
      data_type = x$data_type[1],
      only_old_stations = sum(x$old_rows > 0 & x$new_rows == 0),
      only_new_stations = sum(x$new_rows > 0 & x$old_rows == 0),
      stations_in_both = sum(x$new_rows > 0 & x$old_rows > 0)
    )
  }))
  only_old <- sort(unique(comparison$station_key[
    comparison$old_rows > 0 & comparison$new_rows == 0
  ]))
  only_new <- sort(unique(comparison$station_key[
    comparison$new_rows > 0 & comparison$old_rows == 0
  ]))
  validation_2024 <- read.csv(
    file.path(result_dir, "raw_download_validation_2024.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  currently_unavailable_old <- intersect(
    only_old,
    unique(validation_2024$file_key[
      validation_2024$result == "ministry_reported_no_data"
    ])
  )
  for (i in seq_len(nrow(station_set))) {
    x <- station_set[i, ]
    lines <- c(lines, sprintf(
      "- %s istasyonları: yalnız eski %d, yalnız yeni %d, her ikisinde %d",
      x$data_type, x$only_old_stations, x$only_new_stations, x$stations_in_both
    ))
  }
  lines <- c(
    lines, "", "## 2024 veri bulunan istasyon seti farkları", "",
    paste0("- Yalnız eski scrape'te veri bulunanlar: ", paste(only_old, collapse = ", ")),
    paste0("- Yalnız yeni scrape'te veri bulunanlar: ", paste(only_new, collapse = ", ")),
    paste0(
      "- Eski scrape'te verisi olup Bakanlığın bugün aynı 2024 sorgusuna veri-yok döndürdüğü istasyonlar: ",
      paste(currently_unavailable_old, collapse = ", ")
    ),
    "",
    "Bu fark, bugünkü veri-yok yanıtının verinin geçmişte hiç var olmadığını kanıtlamadığını gösterir; Bakanlık arşivi sonradan değişmiş olabilir."
  )

  band_groups <- split(
    threshold_band,
    interaction(threshold_band$source, threshold_band$year, drop = TRUE)
  )
  lines <- c(
    lines, "", "## %89,5 ve %90 farkı", ""
  )
  for (group in band_groups) {
    lines <- c(lines, sprintf(
      "- %s %d: %d ek istasyon-parametre-tür kombinasyonu, %d benzersiz istasyon",
      group$source[1], group$year[1], nrow(group), length(unique(group$station_key))
    ))
  }
  lines <- c(
    lines,
    "",
    "Not: %89,5 sonucu yuvarlanmış %90 değil, ham doluluk yüzdesine uygulanan ayrı bir eşiktir.",
    "", "Ayrıntılar CSV dosyalarında ve şema içindeki karşılaştırma tablolarındadır."
  )
  writeLines(lines, report_path, useBytes = TRUE)

  invisible(list(
    comparison = comparison, availability = availability,
    threshold_summary = threshold_summary, threshold_delta = threshold_delta,
    threshold_band = threshold_band, comparison_parameter = comparison_parameter,
    comparison_path = comparison_path, availability_path = availability_path,
    threshold_path = threshold_path, delta_path = delta_path, band_path = band_path,
    parameter_path = parameter_path,
    report_path = report_path
  ))
}
