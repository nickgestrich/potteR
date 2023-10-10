#' A function to count missing values across a dataframe.
#'
#' @param data A dataframe or tibble.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' guinea[[3]] |> count_missing()
#'
count_missing <- function(data){
  data |>
    dplyr::summarise(across(tidyselect::everything(), ~ sum(is.na(.)))) |>
    tidyr::pivot_longer(tidyselect::everything(), names_to = "var", values_to = "n" ) |>
    dplyr::mutate("%" = n/nrow(shape_data) * 100) |>
    dplyr::arrange(dplyr::desc(n))
}
