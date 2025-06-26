#' @importFrom rlang arg_match
#' 
#' @keywords internal
#' @noRd
.check_shock_input <- function(var,
                               type,
                               value,
                               file,
                               set,
                               ele,
                               call) {
  if (missing(x = var)) {
    .cli_action(
      msg = "{.arg var} must be provided for all shocks.",
      action = "abort",
      call = call
    )
  }

  if (missing(x = type)) {
    .cli_action(
      msg = "{.arg type} must be provided for all shocks.",
      action = "abort",
      call = call
    )
  } else {
    rlang::arg_match(
      arg = type,
      values = c("uniform", "custom", "scenario"),
      error_call = call
    )
  }

  if (identical(x = type, y = "uniform")) {
    if (missing(x = value) || !is.numeric(x = value)) {
      .cli_action(
        msg = "{.arg value} must be provided a numeric value for all
                  {.arg uniform} shocks.",
        action = "abort",
        call = call
      )
    }
  }

  # data.frame here
  if (is.element(el = type, set = c("custom", "scenario"))) {
    if (is.null(x = file)) {
      .cli_action(
        msg = "{.arg file} must be provided for for shock types {.val custom} and {.val scenario}.",
        action = "abort",
        call = call
      )
    } else if (!file.exists(file)) {
      .cli_action(
        msg = "{.arg file} does not exist.",
        action = "abort",
        call = call
      )
    }

    value <- read.csv(file = file)
    value_colnames <- colnames(x = value)
    if (!is.element(el = "Value", set = value_colnames)) {
      .cli_action(
        msg = "No {.field Value} column was found in the loaded
                  file {.file {file}}.",
        action = "abort",
        call = call
      )
    }
    set <- setdiff(x = value_colnames, y = "Value")
  }
  return(invisible(NULL))
}