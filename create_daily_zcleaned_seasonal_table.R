#!/usr/bin/env Rscript
# Create daily aggregated versions of the seasonal cleaned 2024 hourly tables
# Creates two daily tables:
# - daily_detail_zcleaned_seasonal (2024 seasonal outlier detection)
# - daily_detail_zcleaned_2024_full (2024 full outlier detection for comparison)
# Uses Tarih_NOTZ (timestamp without timezone) for daily aggregation 
# Includes all dates even if no hourly data exists (values will be NULL)

library(DBI)

# 1) Connect to database
message("Connecting to PostgreSQL database...")
con <- tryCatch(
  temizhavaR:::create_postgres_conn(),
  error = function(e) stop("DB bağlantısı başarısız: ", e$message)
)

# Ensure working in public schema
dbExecute(con, "SET search_path TO public;")

# 2) Create daily seasonal table
message("Creating daily_detail_zcleaned_seasonal table...")

# Drop existing table
message("Dropping existing daily_detail_zcleaned_seasonal (if any)...")
dbExecute(con, "DROP TABLE IF EXISTS public.daily_detail_zcleaned_seasonal CASCADE;")

message("Dropping any leftover type named daily_detail_zcleaned_seasonal (if any)...")
dbExecute(con, "DROP TYPE IF EXISTS daily_detail_zcleaned_seasonal CASCADE;")

# Create daily seasonal table for 2024
message("Creating daily_detail_zcleaned_seasonal from hourly_detail_zcleaned_seasonal...")

daily_seasonal_sql <- "
  WITH dates AS (
    SELECT generate_series('2024-01-01'::date, '2024-12-31'::date, '1 day') AS day
  ),
  stations AS (
    SELECT DISTINCT \"location_id\", \"Istasyon_modified\"
    FROM public.hourly_detail_zcleaned_seasonal
  ),
  days_stations AS (
    SELECT day, \"location_id\", \"Istasyon_modified\" FROM dates CROSS JOIN stations
  ),
  agg AS (
    SELECT
      date_trunc('day', \"Tarih_NOTZ\") AS day,
      \"location_id\",
      \"Istasyon_modified\",
      AVG(\"PM10\") AS \"PM10\",
      AVG(\"PM25\") AS \"PM25\",
      AVG(\"SO2\") AS \"SO2\",
      AVG(\"CO\")  AS \"CO\",
      AVG(\"NO2\") AS \"NO2\",
      AVG(\"NOX\") AS \"NOX\",
      AVG(\"NO\")  AS \"NO\",
      AVG(\"O3\")  AS \"O3\"
    FROM public.hourly_detail_zcleaned_seasonal
    WHERE \"Tarih_NOTZ\"::date BETWEEN '2024-01-01' AND '2024-12-31'
    GROUP BY 1, 2, 3
  )
  SELECT
    ds.\"location_id\",
    ds.\"Istasyon_modified\",
    ds.day::timestamp AS \"Tarih_NOTZ\",
    a.\"PM10\", a.\"PM25\", a.\"SO2\", a.\"CO\",
    a.\"NO2\", a.\"NOX\", a.\"NO\", a.\"O3\"
  FROM days_stations ds
  LEFT JOIN agg a ON ds.day = a.day
    AND ds.\"location_id\" = a.\"location_id\"
    AND ds.\"Istasyon_modified\" = a.\"Istasyon_modified\"
  ORDER BY ds.\"location_id\", ds.day
"

dbExecute(con, paste0("CREATE TABLE public.daily_detail_zcleaned_seasonal AS ", daily_seasonal_sql))
message("Daily seasonal table created successfully")

# 3) Create daily full 2024 comparison table
message("Creating daily_detail_zcleaned_2024_full table...")

# Drop existing table
message("Dropping existing daily_detail_zcleaned_2024_full (if any)...")
dbExecute(con, "DROP TABLE IF EXISTS public.daily_detail_zcleaned_2024_full CASCADE;")

message("Dropping any leftover type named daily_detail_zcleaned_2024_full (if any)...")
dbExecute(con, "DROP TYPE IF EXISTS daily_detail_zcleaned_2024_full CASCADE;")

# Create daily full 2024 table
message("Creating daily_detail_zcleaned_2024_full from hourly_detail_zcleaned_2024_full...")

daily_full_sql <- "
  WITH dates AS (
    SELECT generate_series('2024-01-01'::date, '2024-12-31'::date, '1 day') AS day
  ),
  stations AS (
    SELECT DISTINCT \"location_id\", \"Istasyon_modified\"
    FROM public.hourly_detail_zcleaned_2024_full
  ),
  days_stations AS (
    SELECT day, \"location_id\", \"Istasyon_modified\" FROM dates CROSS JOIN stations
  ),
  agg AS (
    SELECT
      date_trunc('day', \"Tarih_NOTZ\") AS day,
      \"location_id\",
      \"Istasyon_modified\",
      AVG(\"PM10\") AS \"PM10\",
      AVG(\"PM25\") AS \"PM25\",
      AVG(\"SO2\") AS \"SO2\",
      AVG(\"CO\")  AS \"CO\",
      AVG(\"NO2\") AS \"NO2\",
      AVG(\"NOX\") AS \"NOX\",
      AVG(\"NO\")  AS \"NO\",
      AVG(\"O3\")  AS \"O3\"
    FROM public.hourly_detail_zcleaned_2024_full
    WHERE \"Tarih_NOTZ\"::date BETWEEN '2024-01-01' AND '2024-12-31'
    GROUP BY 1, 2, 3
  )
  SELECT
    ds.\"location_id\",
    ds.\"Istasyon_modified\",
    ds.day::timestamp AS \"Tarih_NOTZ\",
    a.\"PM10\", a.\"PM25\", a.\"SO2\", a.\"CO\",
    a.\"NO2\", a.\"NOX\", a.\"NO\", a.\"O3\"
  FROM days_stations ds
  LEFT JOIN agg a ON ds.day = a.day
    AND ds.\"location_id\" = a.\"location_id\"
    AND ds.\"Istasyon_modified\" = a.\"Istasyon_modified\"
  ORDER BY ds.\"location_id\", ds.day
"

dbExecute(con, paste0("CREATE TABLE public.daily_detail_zcleaned_2024_full AS ", daily_full_sql))
message("Daily full 2024 comparison table created successfully")

# 4) Add indexes for performance
message("Creating indexes...")

# Indexes for seasonal table
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_seasonal_date ON public.daily_detail_zcleaned_seasonal (\"Tarih_NOTZ\");")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_seasonal_station ON public.daily_detail_zcleaned_seasonal (\"location_id\");")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_seasonal_istasyon ON public.daily_detail_zcleaned_seasonal (\"Istasyon_modified\");")

# Indexes for full 2024 table
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_2024_full_date ON public.daily_detail_zcleaned_2024_full (\"Tarih_NOTZ\");")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_2024_full_station ON public.daily_detail_zcleaned_2024_full (\"location_id\");")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_daily_2024_full_istasyon ON public.daily_detail_zcleaned_2024_full (\"Istasyon_modified\");")

message("Indexes created successfully")

# 5) Verification and reporting
message("\n=== DAILY TABLE VERIFICATION ===")

# Check seasonal table
seasonal_count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM public.daily_detail_zcleaned_seasonal")
message(sprintf("daily_detail_zcleaned_seasonal: %g rows", as.numeric(seasonal_count$n)))

# Check full 2024 table
full_count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM public.daily_detail_zcleaned_2024_full")
message(sprintf("daily_detail_zcleaned_2024_full: %g rows", as.numeric(full_count$n)))

# Sample data verification
message("\n=== SAMPLE DATA VERIFICATION ===")

# Check date range for seasonal table
seasonal_dates <- dbGetQuery(con, "
  SELECT 
    MIN(\"Tarih_NOTZ\") as min_date,
    MAX(\"Tarih_NOTZ\") as max_date,
    COUNT(DISTINCT \"Tarih_NOTZ\") as unique_dates
  FROM public.daily_detail_zcleaned_seasonal
")
message(sprintf("Seasonal table - Date range: %s to %s (%g unique dates)", 
                seasonal_dates$min_date, seasonal_dates$max_date, as.numeric(seasonal_dates$unique_dates)))

# Check date range for full 2024 table
full_dates <- dbGetQuery(con, "
  SELECT 
    MIN(\"Tarih_NOTZ\") as min_date,
    MAX(\"Tarih_NOTZ\") as max_date,
    COUNT(DISTINCT \"Tarih_NOTZ\") as unique_dates
  FROM public.daily_detail_zcleaned_2024_full
")
message(sprintf("Full 2024 table - Date range: %s to %s (%g unique dates)", 
                full_dates$min_date, full_dates$max_date, as.numeric(full_dates$unique_dates)))

# Check station counts
seasonal_stations <- dbGetQuery(con, "SELECT COUNT(DISTINCT \"Istasyon_modified\") as n FROM public.daily_detail_zcleaned_seasonal")
full_stations <- dbGetQuery(con, "SELECT COUNT(DISTINCT \"Istasyon_modified\") as n FROM public.daily_detail_zcleaned_2024_full")
message(sprintf("Seasonal table stations: %g", as.numeric(seasonal_stations$n)))
message(sprintf("Full 2024 table stations: %g", as.numeric(full_stations$n)))

# Sample data quality check
message("\n=== DATA QUALITY CHECK ===")

seasonal_pollutants <- c("O3", "SO2", "PM10", "PM25")
for(p in seasonal_pollutants) {
  # Seasonal table data availability
  seasonal_availability <- dbGetQuery(con, sprintf("
    SELECT 
      COUNT(CASE WHEN \"%s\" IS NOT NULL THEN 1 END) as available,
      COUNT(*) as total,
      ROUND(COUNT(CASE WHEN \"%s\" IS NOT NULL THEN 1 END) * 100.0 / COUNT(*), 2) as percentage
    FROM public.daily_detail_zcleaned_seasonal
  ", p, p))
  
  # Full 2024 table data availability
  full_availability <- dbGetQuery(con, sprintf("
    SELECT 
      COUNT(CASE WHEN \"%s\" IS NOT NULL THEN 1 END) as available,
      COUNT(*) as total,
      ROUND(COUNT(CASE WHEN \"%s\" IS NOT NULL THEN 1 END) * 100.0 / COUNT(*), 2) as percentage
    FROM public.daily_detail_zcleaned_2024_full
  ", p, p))
  
  message(sprintf("%s - Seasonal: %g/%g (%.1f%%), Full 2024: %g/%g (%.1f%%)",
                  p, 
                  as.numeric(seasonal_availability$available), as.numeric(seasonal_availability$total), as.numeric(seasonal_availability$percentage),
                  as.numeric(full_availability$available), as.numeric(full_availability$total), as.numeric(full_availability$percentage)))
}

message("\n=== SUMMARY ===")
message("Daily tables created successfully:")
message("- daily_detail_zcleaned_seasonal: Daily averages from seasonal outlier-cleaned hourly data (2024)")
message("  * O3: Seasonal period Nisan-Eylül (April-September)")
message("  * SO2, PM10, PM2.5: Seasonal period Ocak-Mart + Ekim-Aralık (Jan-Mar + Oct-Dec)")
message("- daily_detail_zcleaned_2024_full: Daily averages from full-year outlier-cleaned hourly data (2024)")
message("  * All pollutants processed for entire 2024 year")
message("")
message("Tables include:")
message("- All 2024 dates (366 days including leap year)")
message("- All stations from source hourly tables")
message("- Daily averages calculated from available hourly data")
message("- NULL values where no hourly data exists")
message("- Proper indexes for performance")
message("")
message("Column names and structure match existing daily tables:")
message("- location_id, Istasyon_modified, Tarih_NOTZ")
message("- PM10, PM25, SO2, CO, NO2, NOX, NO, O3")
message("")
message("Ready for daily analysis and comparison with original daily_detail_zcleaned table")

dbDisconnect(con)
