#' @keywords internal
#' @noRd
.add_REG <- function(input,
                     REG) {
  arr_dimnames <- dimnames(input)[[1]]
  arr_names <- names(dimnames(input))
  input <- array(input,
                 c(length(input), length(REG)),
                 list(arr_dimnames, REG))
  names(dimnames(input)) <- c(arr_names, "REG")
  return(input)
}