#' @importFrom rlang list2 trace_back
#'
#' @export
teems_shock.uniform <- function(var,
                                type,
                                input,
                                ...) {
  call <- rlang::trace_back()$call[[1]]
  if (!missing(...)) {
    subset <- rlang::list2(...)
    if (length(subset) > 0 && (is.null(names(subset)) || any(names(subset) == ""))) {
      .cli_action(shk_err$uni_named_lst,
        shk_infm$uni_named_lst,
        action = c("abort", "inform"),
        url = shk_url$type,
        hyperlink = NULL
      )
    }
  } else {
    subset <- NA
  }
  args_list <- list(
    var = var,
    type = type,
    input = input,
    subset = subset
  )
  config <- .val_uni_shk(args_list, call = call)
  config
}