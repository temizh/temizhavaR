setup_google_auth <- function(email = NULL) {
  tryCatch({
    drive_auth(email = email, scopes = "https://www.googleapis.com/auth/drive")
    gs4_auth(token = drive_token())
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

export_to_google_sheets <- function(data, folder_id, view_name) {
  tryCatch({
    message("A")
    ss <- gs4_create(view_name)
    message("B")
    sheet_write(data, ss = ss, sheet = 1)
    message("C")
    drive_mv(file = as_id(ss), path = as_id(folder_id))
    message(paste0("Exported ", view_name, " to Google Sheets", "folder_id: ", folder_id))
    return(TRUE)
  }, error = function(e) {
    message(e)
    return(FALSE)
  })
}

save_views_to_drive <- function(schema_name, folder_id) {
  message("Authenticating to Google Drive...")
  setup_google_auth()
  
  message("Connecting to the database...")
  # Get the views
  con <- create_postgres_conn()
  views <- dbGetQuery(con, paste0("SELECT table_name FROM information_schema.views WHERE table_schema = '", schema_name, "'"))
  
  message("Exporting views to Google Sheets...")
  for (view in views$table_name) {
    data <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", view))
    export_to_google_sheets(data, folder_id, view)
  }
}