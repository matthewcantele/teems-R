#' @export
teems_shock.scenario <- function(var,
                               type,
                               input,
                               ...) {
  call <- rlang::trace_back()$call[[1]]
  if (!missing(...)) {
    .cli_action(shk_err$unneeded_dots,
                action = "abort",
                call = call)
  }
  args_list <- mget(names(formals()))
  args_list["..."] <- NULL
  config <- .val_scen_shk(args_list = args_list, call = call)
  config
}