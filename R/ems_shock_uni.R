#' @importFrom rlang list2 trace_back
#' @method ems_shock uniform
#' 
#' @export
ems_shock.uniform <- function(var,
                              type,
                              value,
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
    subset <- NULL
  }
  shock <- list(
    var = var,
    type = type,
    input = value,
    subset = subset
  )
  config <- .val_uni_shk(shock = shock, call = call)
  config
}