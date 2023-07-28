#' Make a barplot of categorical count data in a list
#'
#' @param data A list of count data, as created by potteR::sum_categoricals().
#'
#' @return A list of barplots.
#' @importFrom rlang .data
#' @importFrom stats reorder
#' @export
#'
#'
#' @examples
#' guinea[[1]] |>
#'   sum_categoricals() |>
#'   plot_categoricals()
plot_categoricals <- function(data) {
  out <- vector("list", length = length(data))
  for (i in seq_along(data)) {
    out[[i]] <- data[[i]] |>
      dplyr::rename(group = 1) |>
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(.data$group, -.data$n), y = .data$n)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = paste(names(data)[i]),
        x = ""
      )
  }
  out
}
