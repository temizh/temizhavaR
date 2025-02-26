library(DBI)
library(dotenv)

# Set working directory to the directory of the current script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load environment variables from the .env file
dotenv::load_dot_env()

# Read the environment variables
db_host <- Sys.getenv("POSTGRES_HOST")
db_name <- Sys.getenv("TEMIZHAVA_DB")
db_user <- Sys.getenv("POSTGRES_TUSER")
db_password <- Sys.getenv("POSTGRES_TUSER_PASSWORD")
db_port <- Sys.getenv("POSTGRES_PORT")

# Establish a database connection
conn <- tryCatch({
  dbConnect(RPostgres::Postgres(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_password
  )
}, error = function(e) {
  stop("Database connection failed: ", e$message)
})

# Function to check if the connection is still valid
check_db_connection <- function() {
  if (!dbIsValid(conn)) {
    message("Reconnecting to the database...")
    conn <<- dbConnect(RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_password
    )
  }
}