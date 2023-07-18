#' Make a barplot of categorical count data in a list
#'
#' @param data A list of count data, as created by potteR::sum_categoricals().
#'
#' @return A list of barplots.
#' @export
#'
#' @examples
#' guinea[[1]] |>
#' sum_categoricals() |>
#' plot_categoricals()
plot_categoricals <- function(data){
  out <- vector("list", length= length(data))
  for(i in seq_along(data)){
    out[[i]] <- a[[i]] |>
      dplyr::rename(group = 1) |>
      ggplot2::ggplot(aes(x = reorder(group,-n), y = n))+
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::theme_bw()+
      ggplot2::labs(title = paste(names(data)[i]),
                    x = "")
  }
  out
}
