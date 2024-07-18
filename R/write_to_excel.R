#' Save analysis output to Excel file using openxslx
#'
#' @param output The list objects containing analysis result DFs
#' @param output_filename Name of the Excel file to write data in
#' @export



write_output_to_excel <- function(output, output_filename) {
  wb <- createWorkbook()
  for (I in seq_along(output)) {
    # Çıkış listesinde veri olup olmadığını kontrol eder
    if (is.data.frame(output[[I]]$data) && nrow(output[[I]]$data) > 0) {
      addWorksheet(wb, names(output)[I])

      # Boş bir sütun eklenir ve veriler buraya kaydırılır
      output[[I]]$data <- cbind(Task = "", output[[I]]$data)
      output[[I]]$data[1, "Task"] <- output[[I]]$result_message

      hs1 <- createStyle(textDecoration = "Bold", border = "Bottom")
      writeData(wb, names(output)[I], output[[I]]$data, headerStyle = hs1)
      setColWidths(wb, names(output)[I], cols = 1, widths = 50)
      setColWidths(wb, names(output)[I], cols = 2:ncol(output[[I]]$data), widths = 30)
    }
  }

  saveWorkbook(wb, output_filename, overwrite = TRUE)
  print(paste("Wrote to", output_filename))
}
