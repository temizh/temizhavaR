#' @title PostgreSQL Database Connection Functions
#' @description Functions to manage PostgreSQL database connections
#' @name postgres_conns
#' @import DBI
#' @import dplyr
#' @importFrom dotenv load_dot_env
NULL

if (file.exists(".env")) {
  load_dot_env()
} else {
  cat(".env file not found. Please make sure 
  it exists in the root directory.\n")
}

db_host <- Sys.getenv("POSTGRES_HOST")
db_name <- Sys.getenv("TEMIZHAVA_DB")
db_user <- Sys.getenv("POSTGRES_TUSER")
db_password <- Sys.getenv("POSTGRES_TUSER_PASSWORD")
db_port <- Sys.getenv("POSTGRES_PORT")

#' Create a PostgreSQL database connection
#' @return A PostgreSQL connection object
#' @importFrom RPostgres Postgres
#' @export
create_postgres_conn <- function() {
  tryCatch({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = db_name,
                     host = db_host,
                     port = db_port,
                     user = db_user,
                     password = db_password)
    return(con)
  }, error = function(e) {
    message("Failed to connect to database: ", e$message)
    return(NULL)
  })
}

#' Get the list of tables in the PostgreSQL database
#' @param conn The database connection object
#' @return A character vector of table names
#' @export
#' @examples
#' get_postgres_tables(conn)
get_postgres_tables <- function(conn) {
  tryCatch({
    dbListTables(conn)
    print(dbListTables(conn))
  }, error = function(e) {
    message("Error getting table list: ", e$message)
    return(NULL)
  })
}


#' Disconnect from PostgreSQL database
#' @param conn The database connection object
#' @return Boolean indicating success
#' @export
disconnect_postgres <- function(conn) {
  tryCatch({
    if (dbIsValid(conn)) {
      dbDisconnect(conn)
      return(TRUE)
    }
    return(FALSE)
  }, error = function(e) {
    message("Error disconnecting from database: ", e$message)
    return(FALSE)
  })
}

#' Check if database connection is valid
#' @param conn The database connection object
#' @return Boolean indicating if connection is valid
#' @export
is_postgres_connected <- function(conn) {
  tryCatch({
    return(dbIsValid(conn))
  }, error = function(e) {
    return(FALSE)
  })
}

#' Reconnect to PostgreSQL database if connection is lost
#' @param conn The database connection object
#' @return A new connection object or NULL if reconnection fails
#' @export
reconnect_postgres <- function(conn) {
  if (!is_postgres_connected(conn)) {
    message("Connection lost. Attempting to reconnect...")
    disconnect_postgres(conn)
    return(create_postgres_conn())
  }
  return(conn)
}



#' Check how many rows are in a table
#' @param conn The database connection object
#' @param table_name The name of the table
#' @return The number of rows in the table
#' @export
#' @examples
#' get_table_row_count(conn, "daily_detail")
#' get_table_row_count(conn, "hourly_detail")
get_table_row_count <- function(conn, table_name) {
  tryCatch({
    if (!dbIsValid(conn)) {
      conn <- create_postgres_conn()
      if (is.null(conn)) {
        stop("Unable to establish database connection")
      }
    }
    tbl(conn, table_name) %>%
      summarise(n = n()) %>%
      pull(n)
  }, error = function(e) {
    message("Error getting row count: ", e$message)
    return(NULL)
  })
}

# Test 
# conn <- create_postgres_conn()
# get_postgres_tables(conn)

# daily_count <- get_table_row_count(conn, "daily_detail")
# hourly_count <- get_table_row_count(conn, "hourly_detail")

# print(paste("Daily detail rows:", daily_count))
# print(paste("Hourly detail rows:", hourly_count))

# disconnect_postgres(conn)
