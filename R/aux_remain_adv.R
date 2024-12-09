#' Advance Remainder Calculation
#'
#' This function calculates the advanced remainder by removing specified
#' patterns from the 'remainder' elements of a given set. It iterates over each
#' pattern and corresponding remainder, removing the pattern from the remainder,
#' and then trims any leading or trailing whitespace from the result.
#'
#' @param pattern A vector of patterns to be removed from the remainder
#'   elements.
#' @param sets A list or data frame containing the 'remainder' elements from
#'   which the patterns will be removed. The 'remainder' should be a column or
#'   list element within `sets`.
#' @param fixed sub argument
#'
#' @return A vector containing the remainders after the specified patterns have
#'   been removed and the results have been trimmed of whitespace.
#' @importFrom purrr map2
#' @keywords internal
#' @noRd
.advance_remainder <- function(pattern,
                             type,
                             fixed = TRUE) {
  remainder <- get(x = type, envir = parent.frame())[["remainder"]]

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
