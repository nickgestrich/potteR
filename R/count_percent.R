#' Summarise a character vector with counts and percentages
#'
#' @param data A dataframe or tibble
#' @param var A character vector
#'
#' @return a tibble.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' rims <- guinea[[1]]
#' rims |> count_percent(rim_angle)
count_percent <- function(data, var) {
  data|>
    dplyr::count({{var}}) |>
    dplyr::mutate(pct = (.data$n/sum(.data$n)) * 100) |>
    dplyr::arrange(dplyr::desc(.data$pct))
}
