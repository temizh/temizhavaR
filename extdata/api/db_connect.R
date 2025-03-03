library(DBI)
library(dotenv)

# Set working directory to the directory of the current script
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load environment variables from the .env file

#env
base_dir <- getOption("temizhavaR.base_dir")

env_file <- file.path(base_dir, ".env")
if (file.exists(env_file)) {
  dotenv::load_dot_env(file = env_file)
  cat(".env file loaded successfully.\n")
} else {
  stop(".env file not found at: ", env_file)
}


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