% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_categoricals.R
\name{plot_categoricals}
\alias{plot_categoricals}
\title{Make a barplot of categorical count data in a list}
\usage{
plot_categoricals(data)
}
\arguments{
\item{data}{A list of count data, as created by potteR::sum_categoricals().}
}
\value{
A list of barplots.
}
\description{
Make a barplot of categorical count data in a list
}
\examples{
guinea[[1]] |>
  sum_categoricals() |>
  plot_categoricals()
}
