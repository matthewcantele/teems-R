#' @importFrom purrr map2
#' 
#' @keywords internal
#' @noRd
.advance_remainder <- function(remainder,
                               pattern,
                               fixed = TRUE) {

  # NAs mess up the replacement
  remainder <- unlist(x = purrr::map2(
    .x = pattern,
    .y = remainder,
    .f = function(p, rem) {
      if (!is.na(x = p)) {
        trimws(x = sub(pattern = p, replacement = "", x = rem, fixed = fixed))
      } else {
        return(rem)
      }
    }
  ))

  return(remainder)
}
