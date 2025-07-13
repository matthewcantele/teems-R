#' @noRd
#' @keywords internal
.validate_shock <- function(shock,
                            call) {
  if (!.is_shock(shock)) {
    if (!is.list(shock) || !.is_shock(shock[[1]])) {
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