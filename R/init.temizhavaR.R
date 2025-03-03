#' Init the settings for the temizhavaR package
#'
#' @param verbose Print output if TRUE. Default is FALSE
#'
#' @export

init.temizhavaR <- function(verbose = FALSE) {
  raw_dir <- getOption("temizhavaR.raw_dir")


  env_file <- file.path(raw_dir, ".env")
  if (file.exists(env_file)) {
    dotenv::load_dot_env(file = env_file)
    cat(".env file loaded successfully.\n")
  } else {
    stop(".env file not found at: ", env_file)
  }



  if (is.null(raw_dir))
    error("options()$temizhavaR.raw_dir is null. Set your raw data directory correctly")

  if (!dir.exists(raw_dir))
    error(paste("The directory", raw_dir, "does not exist. Please create it and put your raw data files in it."))


  setwd(raw_dir)

  if (verbose) {
    print(paste("raw_dir =", raw_dir))
  }
}

# .onLoad hook
.onLoad <- function(libname, pkgname) {
  init.temizhavaR()
}
