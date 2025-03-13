#' @keywords internal
#' @noRd
.check_closure_inputs <- function(closure_file,
                                  shock_file,
                                  shock,
                                  args_list,
                                  call,
                                  quiet) {
  if (is.null(x = closure_file)) {
    if (!quiet) {
      .cli_action(
        action = "inform",
        msg = "No {.arg closure_file} has been provided so the standard 
        model-specific closure will be used."
      )
    }
  } else if (!file.exists(closure_file)) {
    .cli_action(
      action = "abort",
      msg = "The {.arg closure_file} provided, {.path {closure_file}}, does not 
      exist.",
      call = call
    )
  } else {
    args_list[["closure_file"]] <- path.expand(path = closure_file)
  }

  if (!is.null(x = shock_file)) {
    if (!file.exists(shock_file)) {
      .cli_action(
        action = "abort",
        msg = "The {.arg shock_file} provided, {.path {shock_file}}, does not 
        exist.",
        call = call
      )
    }
    if (!is.null(x = shock)) {
      .cli_action(
        action = "abort",
        msg = "A {.arg shock_file} was provided therefore no additional shocks 
        or modifications are accepted via {.arg shock}.",
        call = call
      )
    }
    args_list[["shock_file"]] <- path.expand(path = shock_file)
  } else {
    if (is.null(x = shock)) {
      if (!quiet) {
        .cli_action("A null shock will return all model coefficients as
                      they are provided and/or calculated in the Tablo file.",
          action = "inform",
          msg = "No {.arg shock} or {.arg shock_file} has been provided so a 
          null shock will be used."
        )
        .cli_action(
          action = "warn",
          msg = "Any significant deviation under these conditions would 
          indicate an error in the loading of input files or parsing of model 
          outputs.",
          call = call
        )
      }
    }
  }
  return(args_list)
}