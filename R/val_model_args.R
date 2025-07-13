#' @importFrom cli cli_verbatim
#' 
#' @keywords internal
#' @noRd
.validate_model_args <- function(args_list,
                                 call) {
  checklist <- list(
    tab_file = "is.character",
    var_omit = c("is.null", "is.character"),
    closure_file = c("is.null", "is.character"),
    swap_in = c("is.null", "is.character", "is.list"),
    swap_out = c("is.null", "is.character", "is.list"),
    shock_file = c("is.null", "is.character"),
    shock = c("is.null", "is.list", "is.data.frame"),
    full_exclude = c("is.character", "is.null"),
    notes = c("is.character", "is.null")
  )

  .check_arg_class(
    args_list = args_list,
    checklist = checklist,
    call = call
  )

  args_list$tab_file <- .check_input(
    file = args_list$tab_file,
    valid_ext = "tab",
    call = call
  )

  if (!is.null(args_list$closure_file)) {
    args_list$closure_file <- .check_input(
      file = args_list$closure_file,
      valid_ext = "cls",
      call = call
    )
  }

  if (!is.null(args_list$shock_file)) {
    if (!is.null(args_list$shock)) {
      .cli_action(shk_err$shk_file_shocks,
        action = "abort",
        call = call
      )
    }

    args_list$shock_file <- .check_input(
      file = args_list$shock_file,
      valid_ext = "shf",
      call = call
    )
  }

  tab_comp <- .process_tablo(
    tab_file = args_list$tab_file,
    var_omit = args_list$var_omit,
    call = call
  )

  args_list$intertemporal <- any(grepl("(intertemporal)", readLines(args_list$tab_file)))

  if (args_list$intertemporal) {
    int_sets <- unlist(subset(tab_comp$set_extract$sets,
      intertemporal,
      select = name
    ))
  }

  if (!is.null(args_list$swap_in)) {
    args_list$swap_in <- lapply(args_list$swap_in,
      .check_swap_input,
      var_extract = tab_comp$var_extract,
      direction = "in",
      call = call
    )
  }

  if (!is.null(args_list$swap_out)) {
    args_list$swap_out <- lapply(args_list$swap_out,
      .check_swap_input,
      var_extract = tab_comp$var_extract,
      direction = "out",
      call = call
    )
  }

  args_list$closure_file <- .check_closure_file(
    closure_file = args_list$closure_file,
    tab_file = args_list$tab_file,
    var_omit = args_list$var_omit,
    var_extract = tab_comp$var_extract,
    quiet = quiet,
    call = call
  )

  if (!is.null(args_list$shock)) {
    args_list$shock <- lapply(
      args_list$shock,
      .check_shock,
      var_extract = tab_comp$var_extract,
      int_sets = int_sets,
      call = call
    )
  }

  if (.o_verbose()) {
    cli::cli_verbatim(tab_comp$summary)
    temporal_dynamics <- if (args_list$intertemporal) {
      "intertemporal"
    } else {
      "static/recursive"
    }
    .cli_action(
      action = "inform",
      msg = "Temporal dynamics have been determined as: {.field {temporal_dynamics}}"
    )
  }

  return(args_list)
}