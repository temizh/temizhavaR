# Run one complete calendar year's analysis.
# Sourcing this file only defines the function; it does not start an analysis.

run_annual_analysis <- function(year, schema_name = NULL,
                                prepare_daily = TRUE,
                                daily = TRUE, hourly = TRUE,
                                aqi_analysis = TRUE,
                                save_to_drive = FALSE,
                                folder_id = NULL,
                                parameters = c("PM10", "PM25", "SO2", "NO2", "O3", "CO"),
                                stations = character()) {
  year <- as.integer(year)
  if (length(year) != 1 || is.na(year) || year < 2000L || year > 2100L) {
    stop("year must be one valid four-digit year")
  }

  if (is.null(schema_name)) schema_name <- paste0("analysis_", year)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", schema_name)) {
    stop("schema_name may contain only letters, numbers and underscores")
  }

  base_dir <- getOption("temizhavaR.base_dir")
  if (is.null(base_dir) || !dir.exists(base_dir)) {
    stop("Set options(temizhavaR.base_dir = '/absolute/data/directory') first")
  }

  source("create_daily_zcleaned_seasonal_year.R", local = .GlobalEnv)
  source("extdata/api/logic.R", local = .GlobalEnv)

  conn <- temizhavaR:::create_postgres_conn()
  if (is.null(conn)) stop("Could not connect to PostgreSQL")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  hourly_source <- paste0("hourly_detail_zcleaned_seasonal_", year)
  if ((daily || hourly) && !DBI::dbExistsTable(conn, hourly_source)) {
    stop(
      "Missing ", hourly_source,
      ". Run create_zcleaned_seasonal_table.R for this year first."
    )
  }

  if (daily || hourly) {
    quoted_hourly_source <- as.character(DBI::dbQuoteIdentifier(conn, hourly_source))
    start_day <- sprintf("%d-01-01", year)
    end_day <- sprintf("%d-01-01", year + 1L)
    expected_days <- as.integer(as.Date(end_day) - as.Date(start_day))
    coverage <- DBI::dbGetQuery(conn, sprintf(
      paste(
        "SELECT COUNT(DISTINCT \"Tarih_NOTZ\"::date) AS days",
        "FROM public.%s",
        "WHERE \"Tarih_NOTZ\" >= '%s'::date AND \"Tarih_NOTZ\" < '%s'::date"
      ),
      quoted_hourly_source, start_day, end_day
    ))
    actual_days <- as.integer(coverage$days[[1]])
    if (is.na(actual_days) || actual_days != expected_days) {
      stop(
        hourly_source, " has ", actual_days, " distinct day(s) for ", year,
        "; expected ", expected_days,
        ". Import/clean the complete year and rerun create_zcleaned_seasonal_table.R."
      )
    }
  }

  schema_exists <- DBI::dbGetQuery(
    conn,
    "SELECT EXISTS (SELECT 1 FROM information_schema.schemata WHERE schema_name = $1) AS found",
    params = list(schema_name)
  )$found[[1]]
  if (isTRUE(schema_exists)) {
    stop("Schema already exists: ", schema_name, ". Choose a new schema_name to preserve existing results.")
  }

  if (daily && prepare_daily) {
    create_daily_zcleaned_seasonal_year(year, conn = conn)
  }
  daily_source <- paste0("daily_detail_zcleaned_seasonal_", year)
  if (daily && !DBI::dbExistsTable(conn, daily_source)) {
    stop("Missing daily source table: ", daily_source)
  }
  aqi_source <- paste0("hourly_detail_zcleaned_", year, "_full")
  if (aqi_analysis && !DBI::dbExistsTable(conn, aqi_source)) {
    stop("AQI analysis requires ", aqi_source)
  }

  start_date <- sprintf("%d-01-01 00:00", year)
  end_date <- sprintf("%d-01-01 00:00", year + 1L)

  create_analysis(
    start_date = start_date,
    end_date = end_date,
    schema_name = schema_name,
    folder_id = folder_id,
    daily = daily,
    hourly = hourly,
    aqi_analysis = aqi_analysis,
    save_to_drive = save_to_drive,
    parameters = parameters,
    stations = stations
  )

  invisible(schema_name)
}
