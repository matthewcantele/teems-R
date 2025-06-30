#' @keywords internal
#' @noRd
.check_class <- function(arg,
                         arg_name,
                         check,
                         sub_arg_name = NULL,
                         call) {
  if (!any(vapply(
    X = check,
    FUN = function(f) match.fun(f)(arg),
    FUN.VALUE = logical(1)
  ))) {
    accepted_class <- substring(
      text = check,
      first = 4,
      last = nchar(x = check)
    )
    accepted_class <- sub(
      pattern = "null",
      replacement = "NULL",
      x = accepted_class
    )

    provided_class <- class(x = arg)
    if (is.null(x = sub_arg_name)) {
      error_msg <- cli::cli_fmt(
      .cli_action(
        msg = gen_err[["class"]],
        action = "abort",
        call = call
      )
      )
    } else {
      .cli_action(
        msg = gen_err[["nested_class"]],
        action = "abort",
        call = call
      )
    }
  }
  return(invisible(NULL))
}
