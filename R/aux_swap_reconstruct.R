#' Reconstruct Swap Structure
#'
#' This function takes a swap object and reconstructs it into a named list where
#' the names are derived from the swap object's elements.
#'
#' @param swap A list containing three elements: variable name, set of names,
#'   and element to swap.
#' @return A list where the element to swap is named by the set of names, and
#'   the list itself is named by the variable name.
#' @keywords internal
#' @noRd
.swap_reconstruct <- function(swap) {

  swap_names <- tail(x = swap, 1)[[1]]
  swap <- head(x = swap, -1)
  names(x = swap) <- swap_names

  f_swap <- swap[["swap_ele"]]
  names(x = f_swap) <- swap[["swap_sets"]]

  f_swap <- list(f_swap)
  names(x = f_swap) <- swap[["var"]]

  return(f_swap)
}
