#' @importFrom cli cli_inform cli_abort
#' 
#' @keywords internal
#' @noRd
.check_closure_inputs <- function(closure_file,
                                  shock_file,
                                  shock,
                                  args_list,
                                  call) {

  if (is.null(x = closure_file)) {
    .dev_trace()
    cli::cli_inform(c("i" = "No {.arg closure_file} has been provided so the standard model-specific closure will be used."))
  } else if (!file.exists(closure_file)) {
    .dev_trace()
    cli::cli_abort(c("x" = "The {.arg closure_file} provided, {.path {closure_file}}, does not exist."))
  } else {
    args_list[["closure_file"]] <- path.expand(path = closure_file)
  }
  
  if (!is.null(x = shock_file)) {
    if (!file.exists(shock_file)) {
      .dev_trace()
      cli::cli_abort(c("x" = "The {.arg shock_file} provided, {.path {shock_file}}, does not exist."))
    }
    if (!is.null(x = shock)) {
      stop("A final `shock_file` was provided therefore no additional shocks or modifications are accepted via `shock`.")
    }
    args_list[["shock_file"]] <- path.expand(path = shock_file)
  } else {
    if (is.null(x = shock)) {
      cli::cli_inform(c(
        "i" = "No {.arg shock} or {.arg shock_file} has been provided so a null shock will be used.",
        "i" = "A null shock will return all model coefficients as
                      they are provided and/or calculated in the Tablo file.",
        "!" = "Any significantion deviation under these
                      conditions would indicate an error in the loading of
                      input files or parsing of model outputs."
      ))
    } 
  }
  return(args_list)
}