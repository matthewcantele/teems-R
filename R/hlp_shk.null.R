#' @noRd
#' @keywords internal
.null_shk <- function(shock) {
  if (is.null(shock)) {
    shocks <- NA
    class(shocks) <- "shock"
    null_shk <- list(
      shocks = shocks,
      shock_file = paste0(
        format(Sys.time(), "%H%M%S"),
        ".shf"
      )
    )
  } else {
    .cli_abort("Internal error on NULL shock.",
      action = "abort",
      .internal = TRUE
    )
  }

  return(null_shk)
}