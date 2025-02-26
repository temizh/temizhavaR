library(DBI)
library(dotenv)

setwd("/home/temizhava/temizhavaR/extdata/DB")

# Load environment variables from the .env file
dotenv::load_dot_env()

# Read the environment variables
db_host <- Sys.getenv("POSTGRES_HOST")
db_name <- Sys.getenv("TEMIZHAVA_DB")
db_user <- Sys.getenv("POSTGRES_TUSER")
db_password <- Sys.getenv("POSTGRES_TUSER_PASSWORD")
db_port <- Sys.getenv("POSTGRES_PORT")

# Connect to the POSTGIS database
postgres_con <- dbConnect(RPostgres::Postgres(),
                          dbname = db_name,
                          host = db_host,
                          port = db_port,
                          user = db_user,
                          password = db_password)

# Connect to the SQLite database
sqlite_con <- dbConnect(RSQLite::SQLite(), "/sqlite/temiz-hava.sqlite")

# Get the list of tables in the SQLite database
tables <- dbListTables(sqlite_con)

# Loop through each table in the SQLite database
for (table in tables) {
  # Read data from the current table into a data frame
  df <- dbReadTable(sqlite_con, table)

  # Write the data frame to the PostgreSQL database
  dbWriteTable(postgres_con, table, df, overwrite = TRUE, row.names = FALSE)

  cat("Transferred table:", table, "\n")
}

# Close the connections
dbDisconnect(sqlite_con)
dbDisconnect(postgres_con)
cat("Migration complete.\n")
