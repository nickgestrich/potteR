#' get summary plots of all variables in a dataset
#'
#' @param data A dataframe or tibble
#'
#' @return A list of plots
#' @export
#'
#' @examples
#' guinea[[3]] |> plot_summary()

plot_summary <- function(data){
  num <- plot_numerics(data)
  cat <- sum_categoricals(data) |>
    plot_categoricals()
  l <- append(cat, list(num), 0)
}
