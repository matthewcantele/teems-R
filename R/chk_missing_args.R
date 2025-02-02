#' @importFrom rlang fn_fmls caller_fn
#' @importFrom cli cli_abort
#' 
#' @keywords internal
#' @noRd
.check_missing_args <- function(call,
                                args_list) {
  args_syms <- rlang::fn_fmls(fn = rlang::caller_fn())
  req_args <- vapply(X = args_syms,
                     FUN = function(arg) {!is.null(arg) && arg == ""},
                     FUN.VALUE = logical(1))
  
  missing_args <- names(x = args_list[req_args & args_list == ""])
  if (length(missing_args) > 0) {
    .dev_trace()
    cli::cli_abort(c("x" = "Missing required arguments: {.arg {missing_args}}.") ,
      call = call
    )
  }
}
