
setwd("/home/nadir/Projects/Pranageo/temizhavaR")
source("/home/nadir/Projects/Pranageo/temizhavaR/.Rprofile", encoding = "UTF-8")

devtools::document()
devtools::build()
devtools::install()

# library(temizhavaR)