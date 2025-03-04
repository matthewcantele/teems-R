#' @keywords internal
#' @noRd
.check_model_version <- function(tab_file,
                                 model_version,
                                 call,
                                 quiet) {
  if (!is.null(x = model_version)) {
    choices <- c("v6.2", "v7.0")
    if (!is.element(el = model_version, set = choices)) {
      .cli_action(action = "abort",
                  msg = "Invalid {.arg model_version}: {.val {model_version}}. Must be {.or {.val {choices}}}.",
                  call = call)
    }
  } else {
    model_version <- .get_model_version(tab_file = tab_file,
                                        call = call,
                                        quiet = quiet)
  }
  return(model_version)
}