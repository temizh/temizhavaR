library(DBI)

# 1) connect
con <- tryCatch(
  temizhavaR:::create_postgres_conn(),
  error = function(e) stop("DB bağlantısı başarısız: ", e$message)
)

# 2) detect source (raw hourly_detail preferred)
tbls <- dbListTables(con)
if ("hourly_detail_cleaned" %in% tbls) {
  src <- "hourly_detail_cleaned"
} else {
  fallback <- tbls[grepl("^hourly_detail_cleaned", tbls, ignore.case=TRUE)]
  if (length(fallback)==0) stop("Kaynak tablo 'hourly_detail_cleaned' bulunamadı")
  # pick first matching if no exact raw table
  src <- fallback[1]
}

# 3) drop and copy source
message("Dropping existing hourly_detail_zcleaned (if any)…")
dbExecute(con, sprintf("DROP TABLE IF EXISTS public.hourly_detail_zcleaned CASCADE;"))

message("Dropping any leftover type named hourly_detail_zcleaned (if any)…")
dbExecute(con, "DROP TYPE IF EXISTS hourly_detail_zcleaned CASCADE;")

message("Copying hourly_detail to hourly_detail_zcleaned using Tarih_NOTZ")
fields <- dbListFields(con, src)
fields_no_tarih <- fields[fields != "Tarih" & fields != "Tarih_NOTZ"]
select_cols <- paste(paste0('"', fields_no_tarih, '"'), collapse = ", ")
select_cols <- paste0(
  select_cols,
  ", \"Tarih_NOTZ\" AS \"Tarih_NOTZ\""
)
dbExecute(con, sprintf(
  "CREATE TABLE public.hourly_detail_zcleaned AS SELECT %s FROM \"%s\";",
  select_cols, src
))
message("Copy complete—using Tarih_NOTZ (Istanbul local time)")

# 4) nullify by Z-score per pollutant on new table
pollutants <- c("PM10","PM25","SO2","CO","NO2","NOX","NO","O3")
for(p in pollutants) {
  message("Nullifying outliers for ", p)
  sql <- sprintf("
    WITH stats AS (
      SELECT
        \"Istasyon_modified\" AS station,
        \"Tarih_NOTZ\"       AS ts,
        \"%1$s\"             AS val,
        AVG(\"%1$s\") OVER (PARTITION BY \"Istasyon_modified\")     AS mu,
        STDDEV_SAMP(\"%1$s\") OVER (PARTITION BY \"Istasyon_modified\") AS sd
      FROM public.hourly_detail_zcleaned
      WHERE \"%1$s\" IS NOT NULL
        AND \"%1$s\" >= 0
    )
    UPDATE public.hourly_detail_zcleaned AS t
    SET \"%1$s\" = NULL
    FROM stats AS s
    WHERE t.\"Istasyon_modified\" = s.station
      AND t.\"Tarih_NOTZ\"       = s.ts
      AND ABS((s.val - s.mu) / NULLIF(s.sd,0)) > 3
  ", p)
  dbExecute(con, sql)
}

# 5) report counts per pollutant
for(p in pollutants) {
  res <- dbGetQuery(con, sprintf("
    SELECT 
      SUM(CASE WHEN \"%1$s\" IS NULL THEN 1 ELSE 0 END) AS n_nullified,
      COUNT(*) AS total
    FROM public.hourly_detail_zcleaned
  ", p))
  message(sprintf("%s: %d outliers nullified of %d total rows",
                  p, as.integer(res$n_nullified), as.integer(res$total)))
}

message("hourly_detail_zcleaned ready and verified")
dbDisconnect(con)
