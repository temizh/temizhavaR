#' Save analysis output to Excel file using openxslx
#'
#' @param output The list objects containing analysis result DFs
#' @param output_filename Name of the Excel file to write data in
#' @export

write_output_to_excel <- function(output, output_filename) {
  wb <- createWorkbook()
  for (I in 1:length(output)) {
    if (length(output[[1]]) > 0 ) {
      addWorksheet(wb, names(output)[I])

      output[[I]]$data <- cbind(output[[I]]$data[,1], output[[I]]$data)
      output[[I]]$data[,1] <- ""
      output[[I]]$data[1,1] <- output[[I]]$result_message
      colnames(output[[I]]$data)[1] <- "Task"

      hs1 <- createStyle(textDecoration = "Bold", border = "Bottom")
      writeData(wb, I, output[[I]]$data, headerStyle = hs1)
      setColWidths(wb, I, cols = 1, widths = 50)
      setColWidths(wb, I, cols = 2:4, widths = 30)
    }
  }

  saveWorkbook(wb, output_filename, overwrite = TRUE)
  print(paste("Wrote to", output_filename))
}
