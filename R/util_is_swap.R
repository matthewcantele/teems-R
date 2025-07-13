#' @keywords internal
.is_swap <- function(x) {
  isTRUE(attr(x, "swap"))
}