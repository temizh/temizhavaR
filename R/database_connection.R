#' Return db connection object
#'
#' @export

db_connection <- function () {
  library(dotenv)

  raw_dir <- getOption("temizhavaR.raw_dir")

  # Load environment variables from the .env file
  dotenv::load_dot_env(file.path(raw_dir, ".env"))

  # Read the environment variables
  db_host <- "dev.pranageo.com"
  db_name <- Sys.getenv("TEMIZHAVA_DB")
  db_user <- Sys.getenv("POSTGRES_TUSER")
  db_password <- Sys.getenv("POSTGRES_TUSER_PASSWORD")
  db_port <-  Sys.getenv("POSTGRES_PORT")

  if (0) {
    mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
  } else {
    # Connect to the POSTGIS database
    mydb <- dbConnect(RPostgres::Postgres(),
                      dbname = db_name,
                      host = db_host,
                      port = db_port,
                      user = db_user,
                      password = db_password)
  }
  mydb
}
