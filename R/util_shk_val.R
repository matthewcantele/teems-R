#' @noRd
#' @keywords internal
.validate_shock <- function(shock,
                            call) {
  if (!inherits(shock, "shock")) {
    if (!is.list(shock) || !inherits(shock[[1]], "shock")) {
      .cli_action(
        shk_err$not_a_shk,
        action = "abort",
        call = call
      )
    } else {
      shock <- shock[[1]]
    }
  }
  return(shock)
}