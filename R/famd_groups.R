#' Form Groups Function
#'
#' This function performs various operations on the provided data, including FAMD (Factor Analysis of Mixed Data) and HCPC (Hierarchical Clustering on Principal Components) clustering. It also optionally generates and prints a cluster plot.
#'
#' @param data A data frame containing the input data.
#' @param column A character vector specifying the columns to be used for FAMD and clustering. Default columns are "diam", "rim_angle", and "simple_rim_type".
#' @param graph Logical. If TRUE, a cluster plot will be generated and printed. Default is FALSE.
#'
#' @return A list containing the following components:
#'   \describe{
#'     \item{complete_data}{A data frame with clustered data.}
#'     \item{famd_input_data}{A data frame with input data used in FAMD.}
#'     \item{results_cluster}{Results of HCPC clustering.}
#'     \item{results_famd}{Results of FAMD analysis.}
#'   }
#'
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom dplyr select filter rename left_join
#' @importFrom FactoMineR FAMD HCPC
#' @importFrom factoextra fviz_cluster
#' @importFrom tidyr all_of everything
#'
#' @examples
#' \dontrun{
#' form_groups(data = my_data, column = c("diam", "rim_angle", "simple_rim_type"), graph = TRUE)
#' }
#'
#' @seealso
#' \code{\link{FactoMineR::FAMD}}
#' \code{\link{FactoMineR::HCPC}}
#' \code{\link{factoextra::fviz_cluster}}
#'
#' @export
famd_groups <- function(data, column = c("diam", "rim_angle", "simple_rim_type"), graph = FALSE){

data <- data
  # Perform FAMD analysis
  res.famd <- data |>
    tibble::column_to_rownames("unit_id") |>
    # selecting the columns for the famd
    dplyr::select(tidyr::all_of(column)) |>
    #removing all rows with na
    dplyr::filter(!dplyr::if_any(tidyr::everything(), is.na)) |>
    FactoMineR::FAMD(graph = graph)

  # Perform HCPC clustering
  res.hcpc <- res.famd |>
    FactoMineR::HCPC(graph = FALSE)

  # Generate and print a cluster plot
  plot_clust <-  res.hcpc |>
    factoextra::fviz_cluster(geom = "point", main = "Factor map", axes = c(1,2))

  # Rename columns in the clustered data
  data_clust_s  <- res.hcpc$data.clust |>
    dplyr::rename(!!get_first_letters(column) := clust)

  # Add the unit_id column back to the clustered data
  data_clust <- data_clust_s |>
    tibble::rownames_to_column("unit_id")

  # Left join the clustered data with the original data
  data_clust <- data |>
    dplyr::left_join(data_clust)

  # Create a list of dataframes and results
  famd_groups <- list(complete_data = data_clust, famd_input_data = data_clust_s, results_cluster = res.hcpc, results_famd = res.famd)

  # Print the cluster plot if requested
  if(graph){
    print(plot_clust)
  } else {}

  return(famd_groups)
}
