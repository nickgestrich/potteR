#' Get summary statistics for all numeric vectors in a dataset.
#'
#' @param data A dataframe or tibble
#'
#' @return A tibble.
#' @importFrom rlang .data
#' @importFrom tidyselect everything
#' @importFrom stats sd
#' @export
#'
#' @examples
#' guinea[[1]] |> sum_numerics()
sum_numerics <- function(data) {
  data |>
    dplyr::select(where(is.numeric)) |>
    tidyr::pivot_longer(everything()) |>
    dplyr::group_by(.data$name) |>
    tidyr::drop_na() |>
    dplyr::summarise(mean = mean(.data$value),
                     min = min(.data$value),
                     max = max(.data$value),
                     sd = sd(.data$value))
}
