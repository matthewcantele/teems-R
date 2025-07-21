#' @noRd
#' @keywords internal
`%=%` <- function(x, y) {
  identical(x, y)
}

`%|||%` <- function(x, y) {
  if (is.null(x)) y else x
}