#' Init the settings for the temizhavaR package
#'
#' @export

init.temizhavaR <- function() {
  raw_dir <- options()$temizhavaR.raw_dir

  if (is.null(raw_dir))
    error("options()$temizhavaR.raw_dir is null. Set your raw data directory correctly")

  if (!dir.exists(raw_dir))
    error(paste("The directory", raw_dir, "does not exist. Please create it and put your raw data files in it."))

  print(paste("raw_dir =", raw_dir))

  setwd(raw_dir)

  source("./init.temizhavaR_data.R")
  print('source("./init.temizhavaR_data.R")')

}
