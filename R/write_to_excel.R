#' Save analysis output to Excel file using openxslx
#'
#' @param output The list objects containing analysis result DFs
#' @param output_filename Name of the Excel file to write data in
#' @export

write_output_to_excel <- function(output, output_filename) {
  wb <- createWorkbook()
  for (I in 1:length(output)) {
    if (length(output[[I]]) > 0) {
      addWorksheet(wb, names(output)[I])

      # output[[I]]$data'nın bir veri çerçevesi veya liste olup olmadığını kontrol eder
      if (is.list(output[[I]]$data) && !is.data.frame(output[[I]]$data)) {
        # Listeyi veri çerçevesine dönüştür
        output[[I]]$data <- do.call(rbind, lapply(output[[I]]$data, function(x) {
          if (is.list(x)) {
            as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
          } else {
            data.frame(Value = x, stringsAsFactors = FALSE)
          }
        }))
      } else if (!is.data.frame(output[[I]]$data)) {
        # Eğer veri çerçevesi değilse, boş veri çerçevesi ile değiştirir
        output[[I]]$data <- data.frame(Task = "No Data Available")
      }

      # Eğer data elemanının sütunu yoksa, bir sütun ekler
      if (ncol(output[[I]]$data) == 0) {
        output[[I]]$data <- data.frame(Task = "No Data Available")
      }

      # data'nın ilk sütununu boşluk olarak ekleyin ve ilk hücreye result_message ekler
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
