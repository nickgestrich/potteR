#' Remove Numbers from a Column in a Data Frame
#'
#' This function takes a data frame and the name of a column as input. It removes numbers from the specified column using regular expressions and returns the modified data frame.
#'
#' @param data A data frame containing the column to be modified.
#' @param column_name The name of the column from which numbers should be removed.
#'
#' @return A data frame with numbers removed from the specified column.
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ensym
#'
#' @examples
#' \dontrun{
#' data <- data.frame(Name = c("T1c", "S9", "S2"))
#' modified_data <- remove_numbers(data, Name)
#' }
#'
#' @export
add_simple_rim <- function(data, column_name) {
  # Ensure the column_name is evaluated as a symbol
  column_name <- ggplot2::ensym(column_name)

  # Remove numbers from the specified column using regular expressions
  data <- data |>
    dplyr::mutate(simple_rim_type = gsub("([0-9].*)", "", !!column_name))

  return(data)
}
