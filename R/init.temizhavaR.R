#' Init the settings for the temizhavaR package
#'
#' @param verbose Print output if TRUE. Default is FALSE
#'
#' @export

init.temizhavaR <- function(verbose = FALSE) {
  base_dir <- getOption("temizhavaR.base_dir")


  env_file <- file.path(base_dir, ".env")
  if (file.exists(env_file)) {
    dotenv::load_dot_env(file = env_file)
    cat(".env file loaded successfully.\n")
  } else {
    stop(".env file not found at: ", env_file)
  }



  if (is.null(base_dir))
    error("options()$temizhavaR.base_dir is null. Set your raw data directory correctly")

  if (!dir.exists(base_dir))
    error(paste("The directory", base_dir, "does not exist. Please create it and put your raw data files in it."))


  setwd(base_dir)

  if (verbose) {
    print(paste("base_dir =", base_dir))
  }
}

# .onLoad hook
.onLoad <- function(libname, pkgname) {
  init.temizhavaR()
}
