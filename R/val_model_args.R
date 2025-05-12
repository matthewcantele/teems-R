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
  args_list[["intertemporal"]] <- .inform_temp_dyn(
    tab_file = args_list[["tab_file"]],
    quiet = quiet
  )
  # drop model_version (picked up in tab_comp)
  args_list <- args_list[!is.element(el = names(x = args_list), set = "model_version")]
  return(args_list)
}