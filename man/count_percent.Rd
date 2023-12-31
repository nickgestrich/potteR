% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_percent.R
\name{count_percent}
\alias{count_percent}
\title{Summarise a character vector with counts and percentages}
\usage{
count_percent(data, var)
}
\arguments{
\item{data}{A dataframe or tibble}

\item{var}{A character vector}
}
\value{
a tibble.
}
\description{
Summarise a character vector with counts and percentages
}
\examples{
rims <- guinea[[1]]
rims |> count_percent(rim_angle)
}
