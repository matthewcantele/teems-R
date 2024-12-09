#' Get Specific Element from Strings
#'
#' This function splits each string in the input vector by the specified
#' delimiter and returns the element at the specified index from each split
#' result. It allows for the use of Perl-compatible regular expressions.
#'
#' @param input A character vector of strings to be split.
#' @param split The pattern or character to split the strings by.
#' @param index The index of the element to retrieve from each split string.
#' @param perl Logical indicating whether to use Perl-compatible regular
#'   expressions for the split.
#'
#' @return A character vector containing the elements at the specified index
#'   from each split string.
#' @importFrom purrr map
#' @keywords internal
#' @noRd
.get_element <- function(input,
                        split,
                        index,
                        perl = FALSE,
                        fixed = FALSE) {
  ele <- unlist(x = purrr::map(
    sapply(
      X = input,
      FUN = strsplit,
      split = split,
      perl = perl
    ),
    index
  ), use.names = FALSE)

  return(ele)
}
