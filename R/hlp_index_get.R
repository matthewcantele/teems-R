#' @keywords internal
#' @noRd
.get_index <- function(dt) {
  idx <- length(x = setdiff(
    x = colnames(x = dt),
    y = "Value"
  ))

  if (identical(x = idx, y = character(0))) {
    idx <- 0
  }
  return(idx)
}
