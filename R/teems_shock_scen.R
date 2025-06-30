#' @export
teems_shock.scenario <- function(var,
                               type,
                               input,
                               ...) {
  call <- rlang::trace_back()[["call"]][[1]]
  if (!missing(...)) {
    .cli_action("{.arg ...} are only utilized for shock type {.val uniform}.",
                action = "abort",
                call = call)
  }
  args_list <- mget(names(formals()))
  args_list["..."] <- NULL
  args_list <- .val_scen_shk(args_list = args_list,
                             call = call)
  args_list
}