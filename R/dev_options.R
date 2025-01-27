#' @importFrom rlang current_call
#' @importFrom cli cli_inform
#'
#' @keywords internal
#' @noRd
.is_dev_mode <- function() {
  tolower(x = Sys.getenv("TEEMS_DEV_MODE", "false")) == "true"
}

.dev_trace <- function() {
  if (.is_dev_mode()) {
    calls <- sys.calls()[-length(sys.calls())]

    trace_info <- vapply(
      seq_along(calls),
      function(i) paste0(i, ": ", deparse1(calls[[i]])),
      character(1)
    )

    cli::cli_inform(
      paste0("Call ", trace_info),
      call = rlang::current_call())

  }
}
