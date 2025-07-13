#' @keywords internal
.is_shock <- function(x) {
  isTRUE(attr(x, "shock"))
}
