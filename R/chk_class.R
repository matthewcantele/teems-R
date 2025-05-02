#' @keywords internal
#' @noRd
.check_class <- function(args,
                         checklist,
                         call) {

  purrr::pmap(.l = list(args, checklist, names(x = args)),
              .f = function(arg, check, arg_name) {
                if (!any(vapply(X = check,
                                FUN = function(f) match.fun(f)(arg),
                                FUN.VALUE = logical(1)))) {
                  accepted_class <- substring(text = check,
                                              first = 4,
                                              last = nchar(x = check))
                  accepted_class <- sub(pattern = "null",
                                        replacement = "NULL",
                                        x = accepted_class)
                  
                  provided_class <- class(x = arg)
                  .cli_action(msg = "{.arg {arg_name}} must be of type {.or {.val 
                              {accepted_class}}}, not {.val {provided_class}}.",
                              action = "abort",
                              call = call)
                }
              })
  return(invisible(NULL))
}
