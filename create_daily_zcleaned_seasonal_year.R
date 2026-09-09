#!/usr/bin/env Rscript

# Build year-specific daily tables from the year-specific hourly outlier tables.
# This file only defines a function; sourcing it does not modify the database.

create_daily_zcleaned_seasonal_year <- function(year, conn = NULL) {
  year <- as.integer(year)
  if (length(year) != 1 || is.na(year) || year < 2000L || year > 2100L) {
    stop("year must be one valid four-digit year")
  }

  owns_connection <- is.null(conn)
  if (owns_connection) {
    conn <- temizhavaR:::create_postgres_conn()
    if (is.null(conn)) stop("Could not connect to PostgreSQL")
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  start_date <- sprintf("%d-01-01", year)
  end_date <- sprintf("%d-12-31", year)
  sources <- c(
    seasonal = sprintf("hourly_detail_zcleaned_seasonal_%d", year),
    full = sprintf("hourly_detail_zcleaned_%d_full", year)
  )
  targets <- c(
    seasonal = sprintf("daily_detail_zcleaned_seasonal_%d", year),
    full = sprintf("daily_detail_zcleaned_%d_full", year)
  )

  source_exists <- vapply(
    sources,
    function(table_name) DBI::dbExistsTable(conn, table_name),
    logical(1)
  )
  missing_sources <- sources[!source_exists]
  if (length(missing_sources) > 0) {
    stop("Missing hourly source table(s): ", paste(missing_sources, collapse = ", "))
  }

  for (kind in names(sources)) {
    source_table <- sources[[kind]]
    target_table <- targets[[kind]]
    quoted_source <- as.character(DBI::dbQuoteIdentifier(conn, source_table))
    quoted_target <- as.character(DBI::dbQuoteIdentifier(conn, target_table))

    sql <- sprintf(
      paste(
        "WITH dates AS (",
        "  SELECT generate_series('%s'::date, '%s'::date, '1 day') AS day",
        "), stations AS (",
        "  SELECT DISTINCT \"location_id\", \"Istasyon_modified\" FROM public.%s",
        "), days_stations AS (",
        "  SELECT day, \"location_id\", \"Istasyon_modified\" FROM dates CROSS JOIN stations",
        "), agg AS (",
        "  SELECT date_trunc('day', \"Tarih_NOTZ\") AS day, \"location_id\", \"Istasyon_modified\",",
        "    AVG(\"PM10\") AS \"PM10\", AVG(\"PM25\") AS \"PM25\",",
        "    AVG(\"SO2\") AS \"SO2\", AVG(\"CO\") AS \"CO\",",
        "    AVG(\"NO2\") AS \"NO2\", AVG(\"NOX\") AS \"NOX\",",
        "    AVG(\"NO\") AS \"NO\", AVG(\"O3\") AS \"O3\"",
        "  FROM public.%s",
        "  WHERE \"Tarih_NOTZ\" >= '%s'::date",
        "    AND \"Tarih_NOTZ\" < ('%s'::date + INTERVAL '1 day')",
        "  GROUP BY 1, 2, 3",
        ")",
        "SELECT ds.\"location_id\", ds.\"Istasyon_modified\", ds.day::timestamp AS \"Tarih_NOTZ\",",
        "  a.\"PM10\", a.\"PM25\", a.\"SO2\", a.\"CO\", a.\"NO2\", a.\"NOX\", a.\"NO\", a.\"O3\"",
        "FROM days_stations ds LEFT JOIN agg a",
        "  ON ds.day = a.day AND ds.\"location_id\" = a.\"location_id\"",
        "  AND ds.\"Istasyon_modified\" = a.\"Istasyon_modified\"",
        "ORDER BY ds.\"location_id\", ds.day"
      ),
      start_date, end_date, quoted_source, quoted_source, start_date, end_date
    )

    DBI::dbWithTransaction(conn, {
      DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS public.%s CASCADE", quoted_target))
      DBI::dbExecute(conn, sprintf("CREATE TABLE public.%s AS %s", quoted_target, sql))
      DBI::dbExecute(conn, sprintf(
        "CREATE INDEX ON public.%s (\"Tarih_NOTZ\", \"Istasyon_modified\")",
        quoted_target
      ))
    })

    verification <- DBI::dbGetQuery(conn, sprintf(
      "SELECT COUNT(DISTINCT \"Tarih_NOTZ\"::date) AS days FROM public.%s",
      quoted_target
    ))
    expected_days <- as.integer(as.Date(sprintf("%d-01-01", year + 1L)) - as.Date(start_date))
    if (as.integer(verification$days[[1]]) != expected_days) {
      stop(target_table, " has ", verification$days[[1]], " days; expected ", expected_days)
    }
    message("Created ", target_table, " with ", expected_days, " calendar days")
  }

  invisible(targets)
}
