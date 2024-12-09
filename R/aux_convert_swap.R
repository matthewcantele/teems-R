#' @keywords internal
#' @noRd
.convert_swap <- function(swap) {
  if (length(x = swap) == 1) {
    swap <- teems_swap(var = swap)
  } else {
    stop("Use `teems_swap()` to load multiple swaps in the same direction.")
  }
  return(swap)
}
