library(temizhavaR)

# Establish a database connection
conn <- create_postgres_conn()

# Function to check if the connection is still valid
check_db_connection <- function() {
  if (!dbIsValid(conn)) {
    message("Reconnecting to the database...")
    conn <<- create_postgres_conn()
  }
}