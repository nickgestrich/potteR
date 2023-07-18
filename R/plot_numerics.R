#' Make a box and violin plot of all numeric columns in a dataset.
#'
#' @param data A dataframe or tibble.
#'
#' @return A box and violin plot
#' @export
#'
#' @examples
#' plot_numerics(guinea[[3]])
#'
plot_numerics <- function(data){
  data |>
    dplyr::select(where(is.numeric)) |>
    tidyr::pivot_longer(everything(), names_to = "var", values_to = "value") |>
    tidyr::drop_na() |>
    ggplot2::ggplot(aes(x = var, y = value)) +
    ggplot2::geom_violin(aes(fill = var))+
    ggplot2::geom_boxplot(alpha = 0.4)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::labs(title = "summary of numerical variables",
                  subtitle = "plot possibly contains values at various scales")
}
