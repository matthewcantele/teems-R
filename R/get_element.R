#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.get_element <- function(input,
                         split,
                         index,
                         perl = FALSE,
                         fixed = FALSE) {
  ele <- purrr::map_chr(input, function(i) {
    purrr::map_chr(strsplit(i, split = split, perl = perl), index)
  })

  return(ele)
}