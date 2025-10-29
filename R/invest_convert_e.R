#' @keywords internal
#' @noRd
.convert_invest_e <- function(input) {
  arr_names <- names(dimnames(input))
  ACTS_dim <- which(arr_names %in% "ACTS")
  CGDS_dim <- dim(input)
  CGDS_dim[ACTS_dim] <- 1
  CGDS_dimnames <- dimnames(input)
  CGDS_dimnames[ACTS_dim] <- list(ACTS = "CGDS")
  CGDS_data <- array(0, CGDS_dim, CGDS_dimnames)
  input <- .abind(input, CGDS_data, along = ACTS_dim)
  names(dimnames(input)) <- arr_names
  return(input)
}