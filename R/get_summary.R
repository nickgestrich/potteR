#' Get a summary list of all variables
#'
#' @param data A dataframe or tibble.
#'
#' @return A list
#' @export
#'
#' @examples
#' get_summary(guinea[[3]])
get_summary <- function(data) {
  x <- data |> sum_categoricals()
  y <- data |> sum_numerics()
  x <- append(x, list(y), 0)
  names(x)[1] <- "numericals"
  return(x)
}
