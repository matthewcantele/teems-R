#' @importFrom rlang trace_back
#' 
#' @method ems_shock custom
#' @export
ems_shock.custom <- function(var,
                             type,
                             input,
                             ...) {
  call <- rlang::trace_back()$call[[1]]
  if (!missing(...)) {
    .cli_action("{.arg ...} are only utilized for shock type {.val uniform}.",
                action = "abort",
                call = call)
  }
  shock <- mget(names(formals()))
  shock["..."] <- NULL
  config <- .val_cust_shk(shock = shock,
                          call = call)
  config
}