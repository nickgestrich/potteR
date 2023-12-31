#' Generate a pdf report on all the variables in a dataset
#'
#' @param data A dataframe or tibble.
#' @param file A filepath to a .pdf document
#'
#' @return A pdf document.
#' @importFrom here here
#' @importFrom gridExtra tableGrob
#' @export
#'
#' @examples
#' \dontrun{
#' guinea[[3]] |> print_summary(file = here::here("summary.pdf"))
#' }
print_summary <- function(data, file) {
  cat("---
title: \"Summary of variables`r paste(data)`\"
date: '`r strftime(Sys.time())`'
output:
  pdf_document
---

\`\`\`{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, cache = TRUE)
\`\`\`


\`\`\`{r example, results='asis'}

library(patchwork)

l <- potteR::plot_summary(data)
x <- potteR::get_summary(data)

out <- vector('list', length = length(l))

for(i in seq_along(l)) {
out[[i]] <-
l[[i]] + gridExtra::tableGrob(x[[i]] |> dplyr::slice_head(n = 15), rows = NULL)
}

out
\`\`\`",
    file = "tmp.Rmd"
  )
  rmarkdown::render("tmp.Rmd", output_file = file)
}
