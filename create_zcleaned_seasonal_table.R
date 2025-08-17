library(DBI)

# Seasonal Z-score outlier detection for 2022 data only
# Ozon: Nisan - Eylül (April - September) 
# SO2, PM10, PM2.5: Ocak - Mart + Ekim - Aralık (January - March + October - December)
# Creates separate tables to preserve existing clean datasets

# 1) Connect to database
con <- tryCatch(
  temizhavaR:::create_postgres_conn(),
  error = function(e) stop("DB bağlantısı başarısız: ", e$message)
)

# 2) Detect source table
tbls <- dbListTables(con)
if ("hourly_detail_cleaned" %in% tbls) {
  src <- "hourly_detail_cleaned"
} else {
  fallback <- tbls[grepl("^hourly_detail_cleaned", tbls, ignore.case=TRUE)]
  if (length(fallback)==0) stop("Kaynak tablo 'hourly_detail_cleaned' bulunamadı")
  src <- fallback[1]
}

# 3) Define years to process
years_to_process <- c(2022, 2023, 2024)

# Get field structure for table creation
fields <- dbListFields(con, src)
fields_no_tarih <- fields[fields != "Tarih" & fields != "Tarih_NOTZ"]
select_cols <- paste(paste0('"', fields_no_tarih, '"'), collapse = ", ")
select_cols <- paste0(
  select_cols,
  ', "Tarih_NOTZ" AS "Tarih_NOTZ"'
)

# Create seasonal outlier tables for each year
for(year in years_to_process) {
  table_name <- paste0("hourly_detail_zcleaned_seasonal_", year)
  
  message("Dropping existing ", table_name, " (if any)…")
  dbExecute(con, sprintf("DROP TABLE IF EXISTS public.%s CASCADE;", table_name))
  
  message("Dropping any leftover type named ", table_name, " (if any)…")
  dbExecute(con, sprintf("DROP TYPE IF EXISTS %s CASCADE;", table_name))
  
  message("Creating ", table_name, " from ", src, " (", year, " data only)")
  dbExecute(con, sprintf(
    "CREATE TABLE public.%s AS SELECT %s FROM \"%s\" WHERE EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %d;",
    table_name, select_cols, src, year
  ))
  message("Copy complete for ", year, "—using Tarih_NOTZ (Istanbul local time)")
}

# 4) Create full comparison tables for each year
for(year in years_to_process) {
  full_table_name <- paste0("hourly_detail_zcleaned_", year, "_full")
  
  message("Creating full ", year, " comparison table...")
  message("Dropping existing ", full_table_name, " (if any)…")
  dbExecute(con, sprintf("DROP TABLE IF EXISTS public.%s CASCADE;", full_table_name))
  
  message("Dropping any leftover type named ", full_table_name, " (if any)…")
  dbExecute(con, sprintf("DROP TYPE IF EXISTS %s CASCADE;", full_table_name))
  
  message("Creating ", full_table_name, " from ", src, " (", year, " data only)")
  dbExecute(con, sprintf(
    "CREATE TABLE public.%s AS SELECT %s FROM \"%s\" WHERE EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %d;",
    full_table_name, select_cols, src, year
  ))
  message("Full ", year, " comparison table created")
}

# 5) Define pollutant seasonal configurations
seasonal_config <- list(
  "O3" = list(
    name = "O3",
    months = c(4:9),  # Nisan - Eylül (April - September)
    description = "Ozon (Nisan-Eylül)"
  ),
  "SO2" = list(
    name = "SO2", 
    months = c(1:3, 10:12),  # Ocak-Mart + Ekim-Aralık
    description = "SO2 (Ocak-Mart + Ekim-Aralık)"
  ),
  "PM10" = list(
    name = "PM10",
    months = c(1:3, 10:12),  # Ocak-Mart + Ekim-Aralık
    description = "PM10 (Ocak-Mart + Ekim-Aralık)"
  ),
  "PM25" = list(
    name = "PM25",
    months = c(1:3, 10:12),  # Ocak-Mart + Ekim-Aralık  
    description = "PM2.5 (Ocak-Mart + Ekim-Aralık)"
  )
)

# 6) Process seasonal outliers for each year separately
message("Processing seasonal outliers for all years...")

for(year in years_to_process) {
  table_name <- paste0("hourly_detail_zcleaned_seasonal_", year)
  message("\n--- Processing seasonal outliers for ", year, " ---")
  
  for(config in seasonal_config) {
    p <- config$name
    months <- config$months
    desc <- config$description
    
    message("Processing ", desc, " for ", year, " - months: ", paste(months, collapse=", "))
    
    # Convert months to SQL IN clause
    months_sql <- paste(months, collapse = ",")
    
    sql <- sprintf("
      WITH seasonal_stats AS (
        SELECT
          \"Istasyon_modified\" AS station,
          \"Tarih_NOTZ\"       AS ts,
          \"%1$s\"             AS val,
          AVG(\"%1$s\") OVER (PARTITION BY \"Istasyon_modified\")     AS mu,
          STDDEV_SAMP(\"%1$s\") OVER (PARTITION BY \"Istasyon_modified\") AS sd
        FROM public.%2$s
        WHERE \"%1$s\" IS NOT NULL
          AND \"%1$s\" >= 0
          AND EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %4$d
          AND EXTRACT(MONTH FROM \"Tarih_NOTZ\") IN (%3$s)
      )
      UPDATE public.%2$s AS t
      SET \"%1$s\" = NULL
      FROM seasonal_stats AS s
      WHERE t.\"Istasyon_modified\" = s.station
        AND t.\"Tarih_NOTZ\"       = s.ts
        AND ABS((s.val - s.mu) / NULLIF(s.sd,0)) > 3
        AND EXTRACT(YEAR FROM t.\"Tarih_NOTZ\") = %4$d
        AND EXTRACT(MONTH FROM t.\"Tarih_NOTZ\") IN (%3$s)
    ", p, table_name, months_sql, year)
    
    dbExecute(con, sql)
  }
}

# 7) Process full outliers for comparison (each year separately)
message("\nProcessing full outliers for comparison...")

comparison_pollutants <- c("O3", "SO2", "PM10", "PM25")
for(year in years_to_process) {
  full_table_name <- paste0("hourly_detail_zcleaned_", year, "_full")
  message("\n--- Processing full ", year, " outliers for comparison ---")
  
  for(p in comparison_pollutants) {
    message("Processing full ", year, " outliers for ", p)
    
    sql <- sprintf("
      WITH full_stats AS (
        SELECT
          \"Istasyon_modified\" AS station,
          \"Tarih_NOTZ\"       AS ts,
          \"%1$s\"             AS val,
          AVG(\"%1$s\") OVER (PARTITION BY \"Istasyon_modified\")     AS mu,
          STDDEV_SAMP(\"%1$s\") OVER (PARTITION BY \"Istasyon_modified\") AS sd
        FROM public.%2$s
        WHERE \"%1$s\" IS NOT NULL
          AND \"%1$s\" >= 0
          AND EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %3$d
      )
      UPDATE public.%2$s AS t
      SET \"%1$s\" = NULL
      FROM full_stats AS s
      WHERE t.\"Istasyon_modified\" = s.station
        AND t.\"Tarih_NOTZ\"       = s.ts
        AND ABS((s.val - s.mu) / NULLIF(s.sd,0)) > 3
        AND EXTRACT(YEAR FROM t.\"Tarih_NOTZ\") = %3$d
    ", p, full_table_name, year)
    
    dbExecute(con, sql)
  }
}

# 8) Report seasonal outlier counts for each year
message("\n=== SEASONAL OUTLIER RESULTS (ALL YEARS) ===")
for(year in years_to_process) {
  table_name <- paste0("hourly_detail_zcleaned_seasonal_", year)
  message("\n--- Results for ", year, " ---")
  
  for(config in seasonal_config) {
    p <- config$name
    desc <- config$description
    months <- config$months
    months_sql <- paste(months, collapse = ",")
    
    res <- dbGetQuery(con, sprintf("
      SELECT 
        SUM(CASE WHEN \"%1$s\" IS NULL THEN 1 ELSE 0 END) AS n_nullified,
        COUNT(*) AS total,
        COUNT(CASE WHEN \"%1$s\" IS NOT NULL THEN 1 END) AS n_valid
      FROM public.%2$s
      WHERE EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %4$d
        AND EXTRACT(MONTH FROM \"Tarih_NOTZ\") IN (%3$s)
    ", p, table_name, months_sql, year))
    
    message(sprintf("%s (%d): %d outliers nullified from %d valid values (%d total rows) in relevant months",
                    desc, year, as.integer(res$n_nullified), as.integer(res$n_valid), as.integer(res$total)))
  }
}

# 9) Report full comparison counts for each year
message("\n=== FULL OUTLIER RESULTS (COMPARISON - ALL YEARS) ===")
for(year in years_to_process) {
  full_table_name <- paste0("hourly_detail_zcleaned_", year, "_full")
  message("\n--- Full ", year, " results ---")
  
  for(p in comparison_pollutants) {
    res <- dbGetQuery(con, sprintf("
      SELECT 
        SUM(CASE WHEN \"%1$s\" IS NULL THEN 1 ELSE 0 END) AS n_nullified,
        COUNT(*) AS total,
        COUNT(CASE WHEN \"%1$s\" IS NOT NULL THEN 1 END) AS n_valid
      FROM public.%2$s
      WHERE EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %3$d
    ", p, full_table_name, year))
    
    message(sprintf("%s (full %d): %d outliers nullified from %d valid values (%d total rows)",
                    p, year, as.integer(res$n_nullified), as.integer(res$n_valid), as.integer(res$total)))
  }
}

# 10) Comparison summary for each year
message("\n=== SEASONAL vs FULL COMPARISON (ALL YEARS) ===")
for(year in years_to_process) {
  table_name <- paste0("hourly_detail_zcleaned_seasonal_", year)
  full_table_name <- paste0("hourly_detail_zcleaned_", year, "_full")
  message("\n--- Comparison for ", year, " ---")
  
  for(config in seasonal_config) {
    p <- config$name
    desc <- config$description
    months <- config$months
    months_sql <- paste(months, collapse = ",")
    
    # Seasonal results
    seasonal_res <- dbGetQuery(con, sprintf("
      SELECT 
        SUM(CASE WHEN \"%1$s\" IS NULL THEN 1 ELSE 0 END) AS n_nullified
      FROM public.%2$s
      WHERE EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %4$d
        AND EXTRACT(MONTH FROM \"Tarih_NOTZ\") IN (%3$s)
    ", p, table_name, months_sql, year))
    
    # Full results  
    full_res <- dbGetQuery(con, sprintf("
      SELECT 
        SUM(CASE WHEN \"%1$s\" IS NULL THEN 1 ELSE 0 END) AS n_nullified
      FROM public.%2$s
      WHERE EXTRACT(YEAR FROM \"Tarih_NOTZ\") = %3$d
    ", p, full_table_name, year))
    
    seasonal_count <- as.integer(seasonal_res$n_nullified)
    full_count <- as.integer(full_res$n_nullified)
    difference <- full_count - seasonal_count
    
    message(sprintf("%s (%d): Seasonal=%d, Full=%d, Difference=%d outliers", 
                    p, year, seasonal_count, full_count, difference))
  }
}

message("\n=== SUMMARY ===")
message("Tables created for each year (2022, 2023, 2024):")
message("- hourly_detail_zcleaned_seasonal_YYYY: Seasonal outlier detection")  
message("  * O3: Nisan-Eylül (April-September)")
message("  * SO2, PM10, PM2.5: Ocak-Mart + Ekim-Aralık (Jan-Mar + Oct-Dec)")
message("- hourly_detail_zcleaned_YYYY_full: Full year outlier detection (comparison)")
message("Original hourly_detail_zcleaned table remains unchanged.")
message("\nTable naming convention:")
for(year in years_to_process) {
  message("  - hourly_detail_zcleaned_seasonal_", year)
  message("  - hourly_detail_zcleaned_", year, "_full")
}

dbDisconnect(con)
