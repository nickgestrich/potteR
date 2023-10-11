#' Get First Letters of Words in a String
#'
#' This function takes an input string, splits it into words, and returns the first letters of each word combined as a single string with "clust_" prefix.
#'
#' @param input_string The input string containing words.
#'
#' @return A character string with the first letters of words combined with "clust_" prefix.
#'
#' @importFrom base paste substr
#' @importFrom utils strsplit unlist
#'
#' @examples
#' get_first_letters("Hello World") # Returns "clust_HW"
#'
#' @export
get_first_letters <- function(input_string) {

   # Split the string into words
  words <- unlist(strsplit(input_string, " "))

  # Extract the first letter of each word
  first_letters <- substr(words, 1, 1)

  # Combine the first letters
  result <- paste(first_letters, collapse = "")

  # Add "clust_" prefix
  result <- paste("clust_", result)

  return(result)
}
