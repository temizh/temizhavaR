.onLoad <- function(libname, pkgname) {
  base_dir <- getOption("temizhavaR.base_dir")
  
  if (is.null(base_dir) || 
      grepl("^/tmp/|^/var/folders/", base_dir) || 
      Sys.getenv("R_PACKAGE_DIR") != "") {
    return(invisible())
  }

  env_file <- file.path(base_dir, ".env")
  if (file.exists(env_file)) {
    tryCatch({
      dotenv::load_dot_env(env_file)
    }, error = function(e) {
      warning("Could not load .env file: ", e$message)
    })
  } else {
    message(env_file, " not found. Please add database variables into .env file in your base_dir")
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("temizhavaR: Loading package dependencies...")
}
