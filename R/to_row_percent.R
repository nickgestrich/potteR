#' Turn count data into percentages by row
#'
#' @param data a dataframe or tibble of count data
#'
#' @return a tibble of percentage data
#' @importFrom tidyselect where
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' x <- tibble::tibble(x = c("a", "b", "c"), y = 4:6, z = 6:8)
#' to_row_percent(x)
#' x |> to_row_percent()

to_row_percent <- function(data){
  data |>
    dplyr::rowwise() |> # make calculations rowwise
    dplyr::mutate(sm = sum(dplyr::c_across(where(is.numeric)))) |> #make row sums...
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.numeric),
                    function(x) (x/.data$sm) * 100)) |>  #...apply in function...
    dplyr::select(-.data$sm)       # ...then delete
}
