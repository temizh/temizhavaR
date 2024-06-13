#' Save analysis output to Excel file using openxslx
#'
#' @param output The list objects containing analysis result DFs
#' @param output_filename Name of the Excel file to write data in
#' @export

write_output_to_excel <- function(output, output_filename) {
  wb <- createWorkbook()
  for (I in 1:length(output)) {
    addWorksheet(wb, names(output)[I])
    writeData(wb, I, output[[I]])  ## write without styling
    setColWidths(wb, I, cols = 1:4, widths = 40)
  }
  saveWorkbook(wb, output_filename, overwrite = TRUE)
  print(paste("Wrote to", output_filename))
}
