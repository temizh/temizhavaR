#' Create an Excel file for analysis
#'
#' @param data The data to be written to Excel
#' @param description The description of the data
#' @param additional Additional views and their descriptions
#' @export

create_analysis_excel <- function(data, description, additional = list()) {
  # add new column to data in front
  if (nrow(data) == 0) {
    data <- data.frame(Task = c(""))
  } else {
    data <- cbind(Task = "", data)
  }
  data[1, "Task"] <- description

  legend <- list()

  i <- 1
  for (addition in additional) {

    letter <- letters[i]
    legend[[i]] <- paste0(letter, ": ", addition$description)

    # rename all columns with year to letter_year and keep Istasyon column
    addition_data <- addition$data %>%
      rename_with(~paste0(letter, "_", .), matches("^20\\d{2}$")) %>%
      select(Istasyon, everything())

    # join addition_data to data
    data <- left_join(data, addition_data, by = "Istasyon")
    i <- i + 1
  }

  # add legend to the first column second row
  if(length(legend) > 0) {
    for (l in 1:length(legend)) {
      data[l + 2, "Task"] <- legend[[l]]
    }
  }

  return(data)
}