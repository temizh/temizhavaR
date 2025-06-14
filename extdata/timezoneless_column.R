library(temizhavaR)
library(lubridate)

conn <- create_postgres_conn()

# conn is your DBI connection
dbExecute(conn, "ALTER TABLE hourly_detail_zcleaned ADD COLUMN \"Tarih_ist\" TIMESTAMP;")

dbExecute(conn, "
  UPDATE hourly_detail_zcleaned
  SET \"Tarih_ist\" = (\"Tarih\" AT TIME ZONE 'Europe/Istanbul');
")