#' @importFrom purrr map
#' 
#' @keywords internal
#' @noRd
.get_element <- function(input,
                        split,
                        index,
                        perl = FALSE,
                        fixed = FALSE) {
  ele <- unlist(x = purrr::map(
    sapply(
      X = input,
      FUN = strsplit,
      split = split,
      perl = perl
    ),
    index
  ), use.names = FALSE)

  return(ele)
}
