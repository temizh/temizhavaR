library(dbplyr)
library(dplyr)
library(DBI)
library(dotenv)
library(temizhavaR)

# Connect to the POSTGIS database
conn <- db_connection()

# Get a list of all tables
(tables <- dbListTables(conn))

# Create a tbl (new data.frame) object that keeps data in the DB server
hrly <- tbl(conn, "hourly_detail")
dly <- tbl(conn, "daily_detail")
location <- tbl(conn, "location")

# Number of stations
# Number of data rows per station
summary <- hrly %>% distinct(Istasyon)
# You can see the actual SQL query
summary %>% show_query()
summary %>% collect() %>% dim()
#COMMENT : there are 329 distinct stations here. But the location table is 330 rows!

# Query to measure performance
query <- 'EXPLAIN ANALYZE SELECT DISTINCT "Istasyon" FROM "hourly_detail";'
before_index <- dbGetQuery(conn, query)
# before Execution Time: 3881.410 ms
# after Execution Time: 3515.268 ms

query <- 'EXPLAIN ANALYZE SELECT DISTINCT "Istasyon" FROM "daily_detail";'
before_index <- dbGetQuery(conn, query)
# before Execution Time: 543.402 ms
# after Execution Time: 185.497 ms

# Create indexes
dbExecute(conn, 'CREATE INDEX idx_daily_istasyon ON daily_detail("Istasyon");')
dbExecute(conn, 'CREATE INDEX idx_hourly_istasyon ON hourly_detail("Istasyon");')
dbExecute(conn, 'CREATE UNIQUE INDEX idx_location_istasyonlar ON location("Istasyonlar");')
# [Todo make location:Istasyonlar unique]

location %>%
  distinct(Istasyonlar) %>%
  anti_join(
    hrly %>% distinct(Istasyon),
    by = c("Istasyonlar" = "Istasyon")
  )

# Number of data rows per station
summary <- hrly %>%
  group_by(Istasyon) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# You can see the actual SQL query
summary %>% show_query()

summary <- summary %>% collect()

#COMMENT : Why these stations have more data?
#1 Çorum                   87808
#2 Adana - Doğankent       87653

summary %>% dim()
# 329 stations

summary %>% print(n = 500)

# Tarih value are not clear
hrly %>% select(Tarih)

dbDisconnect(conn)
