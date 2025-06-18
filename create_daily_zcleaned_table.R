#!/usr/bin/env Rscript
# Create a daily aggregated version of the zcleaned hourly table in Turkey timezone
# Includes all dates even if no hourly data exists (values will be NULL)
# Optimized with indexes and console progress messages

library(DBI)
library(RPostgres)

# 1) Connect to database
message("Connecting to PostgreSQL database...")
# con <- temizhavaR:::create_postgres_conn()
 con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "dev.pranageo.com",
  port = 5435,
  dbname = "temizhava",
  user = "havakalitesi",
  password = "g9JhiHdncd0338"
)
if (is.null(con)) stop("Failed to connect to database")

# Ensure working in public schema
dbExecute(con, "SET search_path TO public;")

# Drop any existing table before starting
message("Dropping existing 'daily_detail_zcleaned' if it exists...")
dbExecute(con, "DROP TABLE IF EXISTS public.daily_detail_zcleaned;")

# 2) Build and execute SQL for daily aggregation
message("Building daily aggregation table 'daily_detail_zcleaned' incrementally by year...")

# retrieve overall date range
range_df <- dbGetQuery(con, 
  "SELECT
     MIN(\"Tarih_NOTZ\") AS min_ts,
     MAX(\"Tarih_NOTZ\") AS max_ts
   FROM public.hourly_detail_zcleaned"
)
min_date <- as.Date(range_df$min_ts)
max_date <- as.Date(range_df$max_ts)
start_year <- as.integer(format(min_date, "%Y"))
end_year   <- as.integer(format(max_date, "%Y"))

# begin transaction
dbExecute(con, "BEGIN;")

for (yr in seq(start_year, end_year)) {
  message("Processing year ", yr)
  y_start <- as.Date(paste0(yr, "-01-01"))
  y_end   <- as.Date(paste0(yr, "-12-31"))
  if (yr == end_year) y_end <- max_date

  # build per-year SQL
  stmt <- sprintf(
    "WITH dates AS (
       SELECT generate_series('%s'::date, '%s'::date, '1 day') AS day
     ),
     stations AS (
       SELECT DISTINCT \"location_id\", \"Istasyon_modified\" AS istasyon_modified
       FROM public.hourly_detail_zcleaned
     ),
     days_stations AS (
       SELECT day, location_id, istasyon_modified FROM dates CROSS JOIN stations
     ),
     agg AS (
       SELECT
         date_trunc('day', \"Tarih_NOTZ\") AS day,
         \"location_id\" AS location_id,
         \"Istasyon_modified\" AS istasyon_modified,
         AVG(\"PM10\") AS \"PM10\",
         AVG(\"PM25\") AS \"PM25\",
         AVG(\"SO2\") AS \"SO2\",
         AVG(\"CO\")  AS \"CO\",
         AVG(\"NO2\") AS \"NO2\",
         AVG(\"NOX\") AS \"NOX\",
         AVG(\"NO\")  AS \"NO\",
         AVG(\"O3\")  AS \"O3\"
       FROM public.hourly_detail_zcleaned
       WHERE \"Tarih_NOTZ\"::date BETWEEN '%s' AND '%s'
       GROUP BY 1,2,3
     )
     SELECT
       ds.location_id,
       ds.istasyon_modified,
       ds.day AS date,
       a.\"PM10\", a.\"PM25\", a.\"SO2\", a.\"CO\",
       a.\"NO2\", a.\"NOX\", a.\"NO\", a.\"O3\"
     FROM days_stations ds
     LEFT JOIN agg a ON ds.day = a.day
       AND ds.location_id = a.location_id
       AND ds.istasyon_modified = a.istasyon_modified",
    y_start, y_end, y_start, y_end
  )

  # first year: create table, else insert
  if (yr == start_year) {
    message("Creating table for year ", yr)
    dbExecute(con, paste0("DROP TABLE IF EXISTS public.daily_detail_zcleaned;"))
    sql_act <- paste0("CREATE TABLE public.daily_detail_zcleaned AS \n", stmt)
  } else {
    message("Inserting year ", yr)
    sql_act <- paste0("INSERT INTO public.daily_detail_zcleaned \n", stmt)
  }

  # execute statement
  message("Executing SQL for year ", yr, "...")
  dbExecute(con, sql_act)
  message("Year ", yr, " processed.")
}

# commit transaction
dbExecute(con, "COMMIT;")
message("Aggregation loop completed and committed.")

# 3) Add indexes for performance
message("Creating indexes...")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_date ON public.daily_detail_zcleaned (date);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_station ON public.daily_detail_zcleaned (location_id);")

# 4) Final verification
message("Final table verification:")
tables <- DBI::dbListTables(con)
message("Available tables in public schema:")
print(tables)
if (!"daily_detail_zcleaned" %in% tables) {
  stop("Error: daily_detail_zcleaned not found after run")
}
rowcount <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM public.daily_detail_zcleaned;")
message("Daily table row count: ", rowcount$n)

# 5) Disconnect
dbDisconnect(con)
message("Database connection closed.")
