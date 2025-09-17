#' @importFrom rlang trace_back
#' 
#' @method ems_shock scenario
#' @export
ems_shock.scenario <- function(var,
                               type,
                               input,
                               ...) {
  call <- rlang::trace_back()$call[[1]]
  if (!missing(...)) {
    .cli_action(shk_err$unneeded_dots,
                action = "abort",
                call = call)
  }
  shock <- mget(names(formals()))
  shock["..."] <- NULL
  config <- .val_scen_shk(shock = shock,
                          call = call)
  config
}