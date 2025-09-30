#' @keywords internal
#' @noRd
.create_v7arr <- function(input,
                          value,
                          ...) {

  set_env <- list(...)
  sets <- unlist(subset(
    coeff_conversion,
    v7.0header == input,
    v7.0set
  ))

  sets <- with(set_env, mget(sets))
  arr <- array(
    data = value,
    dim = lengths(sets),
    dimnames = sets
  )

  class(arr) <- c(input, "par", "v7.0", class(arr))
  return(arr)
}
