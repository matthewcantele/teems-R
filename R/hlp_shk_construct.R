#' @noRd
#' @keywords internal
.construct_shock <- function(raw_shock,
                             closure,
                             sets) {
  UseMethod(".construct_shock")
}
