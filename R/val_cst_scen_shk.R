#' @keywords internal
#' @noRd
.val_cst_scen_shk <- function(shock,
                              call) {

  checklist <- list(
    var = "character",
    type = "character",
    input = c("character", "data.frame")
  )

  .check_arg_class(
    args_list = shock,
    checklist = checklist,
    call = call
  )

  if (inherits(shock$input, "character")) {
    shock$input <- .check_input(
      file = input,
      valid_ext = "csv",
      call = call
    )
    value <- read.csv(shock$input)
    value_colnames <- colnames(value)
    if (!"Value" %=% value_colnames[length(value_colnames)]) {
      .cli_action(
        shk_err$cst_scen_val_file,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    }

    if (shock$type %=% "scenario" && !"Year" %in% value_colnames) {
      .cli_action(
        shk_err$scen_year_file,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    }
  } else if (inherits(shock$input, "data.frame")) {
    value_colnames <- colnames(shock$input)
    if (!"Value" %=% value_colnames[length(value_colnames)]) {
      .cli_action(
        shk_err$cst_scen_val_df,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    }

    if (shock$type %=% "scenario" && !"Year" %in% value_colnames) {
      .cli_action(
        shk_err$scen_year_df,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    }
  }

  shock$set <- setdiff(value_colnames, "Value")
  shock <- structure(shock,
    call = call,
    class = c(shock$type, "shock", class(shock))
  )
  shock <- list(shock)
  return(shock)
}