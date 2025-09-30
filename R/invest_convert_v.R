#' @keywords internal
#' @noRd
.convert_invest_v <- function(input,
                              ...) {

  UseMethod(".convert_invest_v")
}

#' @method .convert_invest_v v7.0
#' @keywords internal
#' @noRd
.convert_invest_v.v7.0 <- function(input,
                              CGDS_data) {

  arr_names <- names(dimnames(input))
  CGDS_dim <- c(dim(CGDS_data), 1)
  CGDS_dimnames <- c(dimnames(CGDS_data), ACTS = "CGDS")
  a_idx <- match(arr_names, names(CGDS_dimnames), 0)
  
  CGDS_dimnames <- CGDS_dimnames[a_idx]
  CGDS_dim <- CGDS_dim[a_idx]
  CGDS_data <- array(CGDS_data, CGDS_dim, CGDS_dimnames)
  bind_dim <- which(names(CGDS_dimnames) %in% "ACTS")
  input <- abind::abind(input, CGDS_data, along = bind_dim)
  names(dimnames(input)) <- arr_names
  return(input)
}

#' @method .convert_invest_v v6.2
#' @keywords internal
#' @noRd
.convert_invest_v.v6.2 <- function(input,
                                   new_h) {

  classes <- class(input)
  classes[1] <- new_h
  PROD_dim <- which(names(dimnames(input)) %in% "PROD_COMM")
  input <- input[, dim(input)[PROD_dim], ]
  class(input) <- unique(c(classes, class(input)))
  return(input)
}