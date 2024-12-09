#' Get Index from Data Table
#'
#' This helper function calculates the index for a given data table by
#' determining the number of columns excluding the 'Value' column. If no other
#' columns are present, the index is set to 0.
#'
#' @param dt A data table for which the index is to be calculated.
#'
#' @return An integer representing the index of the data table, excluding the
#'   'Value' column.
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
