#' @importFrom cli cli_abort
#' @importFrom rlang arg_match
#' 
#' @keywords internal
#' @noRd
.check_shock_input <- function(var,
                               type,
                               file,
                               call) {
  if (!is.element(el = "type", set = names(x = call))) {
    .dev_trace()
    cli::cli_abort(c("!" = "{.arg type} must be provided for all shocks."),
                   call = call)
  } else {
    .dev_trace()
    rlang::arg_match(arg = type,
                     values = c("uniform","custom", "scenario"),
                     error_call = call)
  }
  if (missing(x = var)) {
    .dev_trace()
    cli::cli_abort(c("!" = "{.arg var} must be provided for all shocks."),
                   call = call)
  }
  if (is.element(el = type, set = c("custom", "scenario"))) {
    if (is.null(x = file)) {
      .dev_trace()
      cli::cli_abort(c("!" = "{.arg file} must be provided for for shock types {.val custom} and {.val scenario}."),
                     call = call)
    } else if (!file.exists(file)) {
      .dev_trace()
      cli::cli_abort(c("!" = "{.arg file} does not exist."),
                     call = call)
    }
  }
  return(invisible(NULL))
}