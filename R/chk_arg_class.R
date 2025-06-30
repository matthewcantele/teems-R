#' @importFrom purrr pmap map2
#' @importFrom rlang cnd_signal
#' 
#' @keywords internal
.check_arg_class <- function(args_list,
                             checklist,
                             call) {
  withCallingHandlers(
    purrr::pmap(
      .l = list(
        args_list,
        names(x = args_list),
        checklist,
        names(x = checklist)
      ),
      .f = function(arg, arg_name, checks, check_name) {
        if (!identical(x = arg_name, y = check_name)) {
          .cli_action(
            msg = "Check missing from checklist",
            action = "abort",
            .internal = TRUE,
            call = call
          )
        }
        if (!is.list(x = checks)) {
          .check_class(
            arg = arg,
            arg_name = arg_name,
            check = checks,
            call = call
          )
        } else {
          .check_class(
            arg = arg,
            arg_name = arg_name,
            check = checks[[1]],
            call = call
          )
          withCallingHandlers(
            purrr::map2(
              .x = arg,
              .y = names(x = arg),
              .f = function(arg, sub_arg_name) {
                .check_class(
                  arg = arg,
                  arg_name = arg_name,
                  sub_arg_name = sub_arg_name,
                  check = checks[[2]],
                  call = call
                )
              }
            ),
            purrr_error_indexed = function(err) {
              rlang::cnd_signal(cnd = err[["parent"]])
            }
          )
        }
      }
    ),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err[["parent"]])
    }
  )
}
