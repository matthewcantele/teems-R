#' @importFrom cli cli_fmt
#' 
#' @keywords internal
#' @noRd
.check_class <- function(arg,
                         arg_name,
                         check,
                         sub_arg_name = NULL,
                         call) {
  if (!any(vapply(check,
    FUN = function(f) match.fun(f)(arg),
    FUN.VALUE = logical(1)
  ))) {
    accepted_class <- substring(
      check,
      4,
      nchar(check)
    )
    accepted_class <- sub(
      "null",
      "NULL",
      accepted_class
    )

    provided_class <- class(arg)
    if (is.null(sub_arg_name)) {
      error_msg <- cli::cli_fmt(
        .cli_action(
          gen_err$class,
          action = "abort",
          call = call
        )
      )
    } else {
      .cli_action(
        gen_err$nested_class,
        action = "abort",
        call = call
      )
    }
  }
  return(invisible(NULL))
}