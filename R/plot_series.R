#' a wrapper function around tabula::plot_ford() in order to ease data
#' preparation
#'
#' @param data A dataframe or tibble.
#' @param context_var The variable that designates the context or assemblage
#' @param attribute_var The attribute that is the target for assessing seriation
#' @param context_order A vector giving the predefined order for the values in
#' context_var
#'
#' @return A ggplot object
#' @importFrom tabula plot_ford
#' @importFrom rlang .data
#' @export
#'
#'
#' @examples
#' order <- as.character(c(1:26))
#' guinea[[1]] |> plot_series(context, rim_type, order)

plot_series <- function(data, context_var, attribute_var, context_order) {
  data |>
    dplyr::count({{ context_var }}, {{ attribute_var }}) |>
    tidyr::drop_na({{ attribute_var }}) |>
    tidyr::pivot_wider(
      names_from = {{ attribute_var }},
      values_from = .data$n,
      values_fill = list(n = 0)
    ) |>
    dplyr::arrange(factor({{ context_var }}, levels = context_order)) |>
    tibble::column_to_rownames(rlang::englue("{{ context_var }}")) |>
    tabula::plot_ford()
}
