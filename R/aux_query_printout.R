#' @keywords internal
#' @noRd
.query_printout <- function(data) {
  printout <- as.list(x = data)
  printout <- sapply(X = printout,
                     FUN = function(ls_ele) {
                       if (is.list(x = ls_ele)) {
                         ls_ele <- purrr::list_flatten(x = ls_ele)
                       }
                       unname(obj = ls_ele)
                     })
  return(printout)
}
