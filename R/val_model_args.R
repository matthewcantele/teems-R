#' @keywords internal
#' @noRd
.validate_model_args <- function(args_list,
                                 call,
                                 quiet) {
  args_list[["ndigits"]] <- as.integer(x = args_list[["ndigits"]])
  .check_class(
    args = args_list,
    checklist = list(
      tab_file = "is.character",
      var_omit = c("is.null", "is.character"),
      model_version = c("is.null", "is.character"),
      ndigits = "is.integer",
      full_exclude = c("is.character", "is.null"),
      notes = c("is.character", "is.null"),
      quiet = "is.logical"
    ),
    call = call
  )
  args_list[["tab_file"]] <- .check_input(
    file = args_list[["tab_file"]],
    valid_ext = "tab",
    call = call
  )
  args_list[["model_version"]] <- .check_model_version(
    tab_file = args_list[["tab_file"]],
    model_version = args_list[["model_version"]],
    call = call,
    quiet = quiet
  )
  args_list[["intertemporal"]] <- .inform_temp_dyn(
    tab_file = args_list[["tab_file"]],
    quiet = quiet
  )
  return(args_list)
}