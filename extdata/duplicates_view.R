# Create a new view that shows the duplicates in the database

conn <- create_postgres_conn()
dbExecute(conn, "CREATE OR REPLACE VIEW duplicates_view AS
  SELECT \"Istasyon_modified\", \"Tarih_NOTZ\", COUNT(*) AS duplicate_count
  FROM hourly_detail_zcleaned
  GROUP BY \"Istasyon_modified\", \"Tarih_NOTZ\"
  HAVING COUNT(*) > 1;"
)

# close the connection
dbDisconnect(conn)
