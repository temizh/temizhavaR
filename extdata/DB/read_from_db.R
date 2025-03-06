library(dbplyr)
library(dplyr)
library(DBI)
library(dotenv)

# setwd("/home/temizhava/temizhavaR/extdata/DB")

base_dir <- getOption("temizhavaR.base_dir")

env_file <- file.path(base_dir, ".env")
if (file.exists(env_file)) {
  dotenv::load_dot_env(file = env_file)
  cat(".env file loaded successfully.\n")
} else {
  stop(".env file not found at: ", env_file)
}

# Read the environment variables
db_host <- "dev.pranageo.com"
db_name <- Sys.getenv("TEMIZHAVA_DB")
db_user <- Sys.getenv("POSTGRES_TUSER")
db_password <- Sys.getenv("POSTGRES_TUSER_PASSWORD")
db_port <-  Sys.getenv("POSTGRES_PORT")

# Connect to the POSTGIS database
postgres_con <- dbConnect(RPostgres::Postgres(),
                          dbname = db_name,
                          host = db_host,
                          port = db_port,
                          user = db_user,
                          password = db_password)

# Get a list of all tables
(tables <- dbListTables(postgres_con))

# Create a tbl (new data.frame) object that keeps data in the DB server
hrly <- tbl(postgres_con, "hourly_detail")
dly <- tbl(postgres_con, "daily_detail")
location <- tbl(postgres_con, "location")

# Number of stations
# Number of data rows per station
summary <- hrly %>%
    distinct(Istasyon) %>%
    collect()
dim(summary)
#COMMENT : there are 329 distinct stations here. But the location table is 330 rows!

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

dbDisconnect(postgres_con)
