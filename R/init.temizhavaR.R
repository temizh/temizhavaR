#' Init the settings for the temizhavaR package
#'
#' @param verbose Print output if TRUE. Default is FALSE
#'
#' @export

init.temizhavaR <- function(verbose = FALSE) {
  base_dir <- getOption("temizhavaR.base_dir")

  if (is.null(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("options()$temizhavaR.base_dir is not set")
  }

  if (!dir.exists(base_dir)) {
    stop("The directory does not exist: ", base_dir)
  }

  env_file <- file.path(base_dir, ".env")
  if (file.exists(env_file)) {
    dotenv::load_dot_env(file = env_file)
    cat(".env file loaded successfully.\n")
  } else {
    stop(".env file not found at: ", env_file)
  }

  setwd(base_dir)

  if (verbose) {
    print(paste("base_dir =", base_dir))
  }
}
