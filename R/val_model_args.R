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
      closure_file = c("is.null", "is.character"),
      swap_in = c("is.null", "is.character", "is.list"),
      swap_out = c("is.null", "is.character", "is.list"),
      shock_file = c("is.null", "is.character"),
      shock = c("is.null", "is.list"),
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
  .process_tablo(tab_file = args_list[["tab_file"]],
                 var_omit = args_list[["var_omit"]],
                 call = call,
                 quiet = quiet)
  args_list[["intertemporal"]] <- .inform_temp_dyn(
    tab_file = args_list[["tab_file"]],
    quiet = quiet
  )
  
  args_list[["swap_in"]] <- lapply(X = args_list[["swap_in"]],
                                   FUN = .check_usr_swap,
                                   quiet = quiet,
                                   call = call)
  
  args_list[["swap_out"]] <- lapply(X = args_list[["swap_out"]],
                                   FUN = .check_usr_swap,
                                   quiet = quiet,
                                   call = call)

  args_list[["closure_file"]] <- .check_closure_file(
    closure_file = args_list[["closure_file"]],
    tab_file = args_list[["tab_file"]],
    var_omit = args_list[["var_omit"]],
    quiet = quiet,
    call = call
  )
  
  return(args_list)
}