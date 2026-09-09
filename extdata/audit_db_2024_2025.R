library(temizhavaR)
library(DBI)

con <- temizhavaR:::create_postgres_conn()
on.exit(temizhavaR:::disconnect_postgres(con), add = TRUE)

for (table in c("daily_detail", "hourly_detail")) {
  query <- sprintf(
    paste(
      "SELECT EXTRACT(YEAR FROM \"Tarih\")::int AS year,",
      "COUNT(*)::bigint AS rows,",
      "COUNT(DISTINCT \"Istasyon_modified\")::int AS stations,",
      "MIN(\"Tarih\") AS min_date, MAX(\"Tarih\") AS max_date",
      "FROM %s",
      "WHERE \"Tarih\" >= DATE '2024-01-01'",
      "AND \"Tarih\" < DATE '2026-01-01'",
      "GROUP BY 1 ORDER BY 1"
    ),
    table
  )
  cat("TABLE", table, "\n")
  print(dbGetQuery(con, query), row.names = FALSE)
}

if (DBI::dbExistsTable(con, DBI::Id(schema = "raw_20260908", table = "import_manifest"))) {
  for (table in c("daily_detail", "hourly_detail")) {
    cat("SNAPSHOT_SAMPLE", table, "\n")
    print(dbGetQuery(
      con,
      sprintf(
        paste(
          "SELECT COUNT(*)::int AS rows,",
          "MIN(observed_at_local) AS local_min, MAX(observed_at_local) AS local_max,",
          "MIN(observed_at_utc) AS utc_min, MAX(observed_at_utc) AS utc_max,",
          "COUNT(*) - COUNT(DISTINCT observed_at_local) AS duplicate_times",
          "FROM raw_20260908.%s WHERE analysis_year = 2025",
          "AND station_key = 'Adana-Çukurova'"
        ),
        table
      )
    ), row.names = FALSE)
  }
}

cat("PUBLIC_LOCAL_YEAR_2024\n")
print(dbGetQuery(
  con,
  paste(
    "SELECT",
    "(SELECT COUNT(*) FROM daily_detail",
    " WHERE \"Tarih_NOTZ\" >= TIMESTAMP '2024-01-01'",
    " AND \"Tarih_NOTZ\" < TIMESTAMP '2025-01-01') AS daily_rows,",
    "(SELECT COUNT(*) FROM hourly_detail",
    " WHERE \"Tarih_NOTZ\" >= TIMESTAMP '2024-01-01'",
    " AND \"Tarih_NOTZ\" < TIMESTAMP '2025-01-01') AS hourly_rows"
  )
), row.names = FALSE)

cat("CONSTRAINTS\n")
print(dbGetQuery(
  con,
  paste(
    "SELECT tc.table_name, tc.constraint_name, tc.constraint_type",
    "FROM information_schema.table_constraints tc",
    "WHERE tc.table_schema = 'public'",
    "AND tc.table_name IN ('daily_detail', 'hourly_detail')",
    "ORDER BY 1, 2"
  )
), row.names = FALSE)

cat("LOCATION_COLUMNS\n")
print(dbGetQuery(
  con,
  paste(
    "SELECT column_name, data_type FROM information_schema.columns",
    "WHERE table_schema = 'public' AND table_name = 'location' ORDER BY ordinal_position"
  )
), row.names = FALSE)

cat("LOCATION_COUNT\n")
print(dbGetQuery(con, "SELECT COUNT(*)::int AS rows FROM location"), row.names = FALSE)

cat("LOCATION_CONSTRAINTS_AND_ID\n")
print(dbGetQuery(
  con,
  paste(
    "SELECT conname, pg_get_constraintdef(oid) AS definition",
    "FROM pg_constraint WHERE conrelid = 'location'::regclass ORDER BY conname"
  )
), row.names = FALSE)
print(dbGetQuery(
  con,
  paste(
    "SELECT column_default FROM information_schema.columns",
    "WHERE table_schema = 'public' AND table_name = 'location' AND column_name = 'Id'"
  )
), row.names = FALSE)

cat("NEW_STATION_LOCATION_MATCHES\n")
catalogue <- read.csv(
  file.path(getOption("temizhavaR.base_dir"), "station_catalogue_2025.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
new_keys <- catalogue$file_key[catalogue$status == "new_on_site"]
quoted_keys <- paste(dbQuoteString(con, new_keys), collapse = ", ")
print(dbGetQuery(
  con,
  sprintf(
    paste(
      "SELECT \"Id\", \"Sehir\", \"Istasyon_modified\",",
      "\"Istasyon_original\", \"Sampling_Point_Id\"",
      "FROM location WHERE \"Istasyon_modified\" IN (%s)",
      "ORDER BY \"Istasyon_modified\""
    ),
    quoted_keys
  )
), row.names = FALSE)

cat("DETAIL_COLUMNS\n")
print(dbGetQuery(
  con,
  paste(
    "SELECT table_name, column_name, data_type, is_nullable",
    "FROM information_schema.columns",
    "WHERE table_schema = 'public'",
    "AND table_name IN ('daily_detail', 'hourly_detail')",
    "ORDER BY table_name, ordinal_position"
  )
), row.names = FALSE)

cat("CONSTRAINT_DEFINITIONS\n")
print(dbGetQuery(
  con,
  paste(
    "SELECT conrelid::regclass::text AS table_name, conname,",
    "pg_get_constraintdef(oid) AS definition",
    "FROM pg_constraint",
    "WHERE conrelid::regclass::text IN ('daily_detail', 'hourly_detail')",
    "ORDER BY 1, 2"
  )
), row.names = FALSE)

for (table in c("daily_detail", "hourly_detail")) {
  cat("SAMPLE", table, "\n")
  print(dbGetQuery(
    con,
    sprintf(
      paste(
        "SELECT \"Istasyon_modified\", \"Tarih\", \"Tarih_NOTZ\"",
        "FROM %s WHERE \"Istasyon_modified\" = 'Adana-Çukurova'",
        "AND \"Tarih\" >= TIMESTAMPTZ '2023-12-31 00:00:00+00'",
        "ORDER BY \"Tarih\" LIMIT 4"
      ),
      table
    )
  ), row.names = FALSE)
}
