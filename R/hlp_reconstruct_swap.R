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
