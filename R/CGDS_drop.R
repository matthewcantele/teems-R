#' @keywords internal
#' @noRd
.drop_CGDS <- function(input) {
  classes <- class(input)
  arr_dimnames <- dimnames(input)
  PROD_dim <- which(names(arr_dimnames) %in% "PROD_COMM")
  arr_indices <- rep(list(bquote()), length(arr_dimnames))
  PROD_ele <- arr_dimnames[[PROD_dim]]
  arr_indices[[PROD_dim]] <- -length(PROD_ele)
  input <- do.call("[", c(list(input), arr_indices))
  class(input) <- unique(classes, class(input))
  return(input)
}