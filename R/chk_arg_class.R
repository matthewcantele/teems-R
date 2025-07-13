#' @importFrom purrr pmap map2
#' @importFrom rlang cnd_signal
#' 
#' @keywords internal
.check_arg_class <- function(args_list,
                             checklist,
                             call) {
  withCallingHandlers(
    purrr::pmap(
      list(
        args_list,
        names(x = args_list),
        checklist,
        names(x = checklist)
      ),
      function(arg, arg_name, checks, check_name) {
        if (!arg_name %=% check_name) {
          browser()
          .cli_action(
            msg = "Check missing from checklist",
            action = "abort",
            .internal = TRUE,
            call = call
          )
        }
        .check_class(
          arg = arg,
          arg_name = arg_name,
          check = checks,
          call = call
        )
      }
    ),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err[["parent"]])
    }
  )
}
