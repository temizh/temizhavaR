.onLoad <- function(libname, pkgname) {
  if (file.exists(".env")) {
    dotenv::load_dot_env()
  } else {
     print(paste0(getwd(), "/.env not found. Please add database variables into .env file in your base_dir"))
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("temizhavaR: Loading package dependencies...")
}
