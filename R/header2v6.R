#' @keywords internal
#' @noRd
.header2v6 <- function(input) {
  UseMethod(".header2v6")
}

#' @method .header2v6 default
#' @keywords internal
#' @noRd
#' @export
.header2v6.default <- function(input) {
  browser()
}

#' @method .header2v6 VDFB
#' @keywords internal
#' @noRd
#' @export
.header2v6.VDFB <- function(input) {
  browser()
}