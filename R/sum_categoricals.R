#' Make a list of counts and percentages of all categorical vectors in a dataset.
#'
#' @param data A dataframe or tibble.
#'
#' @return A list.
#' @export
#'
#' @examples
#' guinea[[1]] |> sum_categoricals()
sum_categoricals <- function(data) {
  data |>
    dplyr::select(where(is.character)) |>
    purrr::map(count_percent, data = data)
}
