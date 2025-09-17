#' @keywords internal
#' @noRd
.check_class <- function(arg,
                         arg_name,
                         check,
                         sub_arg_name = NULL,
                         call) {
  if (!inherits(arg, check)) {
    .cli_action(
      gen_err$class,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}