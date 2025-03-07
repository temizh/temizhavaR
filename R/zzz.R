.onLoad <- function(libname, pkgname) {
  base_dir <- getOption("temizhavaR.base_dir")

  if (grepl("^/tmp/", base_dir)) {
    message("Skipping .env loading during package installation...")
    return()
  }

  env_file <- file.path(base_dir, ".env")
  if (file.exists(env_file)) {
    dotenv::load_dot_env(env_file)
  } else {
    message(env_file, " not found. Please add database variables into .env file in your base_dir")
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("temizhavaR: Loading package dependencies...")
}
