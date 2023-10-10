#' Make boxplots from all numeric-categrocial pairs
#'
#' @param data A dataframe or tibble.
#'
#' @return A list of ggplot objects.
#' @export
#'
#' @examples
#' p <- all_boxplots(guinea[[3]])
#' print(p)

all_boxplots <- function(data) {

  # get names for the numeric and categorical variables
  catvars <- data |> dplyr::select(tidyselect::where(is.character)) |> names()
  numvars <- data |> dplyr::select(tidyselect::where(is.numeric)) |> names()

  # make df with all combinations
  g <- tidyr::crossing(catvars, numvars)

  # draw boxplots
  apply(g,1,function(x) ggplot2::ggplot(data,aes(x=reorder(.data[[x[1]]], .data[[x[2]]], na.rm = TRUE), y=.data[[x[2]]]))+
          ggplot2::geom_boxplot()+
          ggplot2::theme_bw())

}
