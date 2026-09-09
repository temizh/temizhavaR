library(temizhavaR)

# The API connection is lazy so sourcing analysis helpers has no side effect.
conn <- NULL

# Function to check if the connection is still valid
check_db_connection <- function() {
  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    message("Reconnecting to the database...")
    conn <<- create_postgres_conn()
    if (is.null(conn)) stop("Could not connect to PostgreSQL")
  }
}
