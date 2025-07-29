#' @importFrom purrr pluck
#' @importFrom rlang arg_match
#' 
#' @noRd
#' @keywords internal
.validate_data_args <- function(args_list,
                                call) {
  checklist <- list(
    dat_input = "is.character",
    par_input = "is.character",
    set_input = "is.character",
    aux_input = c("is.null", "is.character"),
    tab_file = c("is.null", "is.character"),
    target_format = c("is.null", "is.character"),
    time_steps = c("is.null", "is.numeric")
  )

  .check_arg_class(
    args_list = args_list,
    checklist = checklist,
    call = call
  )

  args_list$dat_input <- .check_input(args_list$dat_input,
    valid_ext = "har",
    cache = FALSE,
    call = call
  )

  args_list$par_input <- .check_input(args_list$par_input,
    valid_ext = "har",
    cache = FALSE,
    call = call
  )

  args_list$set_input <- .check_input(args_list$set_input,
    valid_ext = "har",
    cache = FALSE,
    call = call
  )
  
  if (!is.null(args_list$aux_input)) {
    args_list$aux_input <- .check_input(args_list$aux_input,
                                        valid_ext = "har",
                                        cache = FALSE,
                                        call = call
    )
    args_list$aux <- .read_har(con = args_list$aux_input)
  }

  if (!is.null(args_list$target_format)) {
    target_format <- args_list$target_format
    args_list$target_format <- rlang::arg_match(
      arg = target_format,
      values = c("v6.2", "v7.0")
    )
    if (is.null(args_list$tab_file)) {
      .cli_action(data_err$missing_tar,
        action = "abort",
        call = call
      )
    }
  }

  if (!is.null(args_list$time_steps) && is.null(args_list$tab_file)) {
    .cli_action(data_err$missing_tab,
      action = "abort",
      call = call
    )
  }

  if (!is.null(args_list$tab_file)) {
    args_list$tab_file <- .check_input(
      file = args_list$tab_file,
      valid_ext = "tab",
      cache = FALSE,
      call = call
    )
  }

  args_list$dat <- .read_har(con = args_list$dat_input)
  args_list$par <- .read_har(con = args_list$par_input)
  args_list$set <- .read_har(con = args_list$set_input)

  if (!inherits(args_list$dat, "dat")) {
    inferred_type <- attr(args_list$dat, "data_type")
    .cli_action(data_err$invalid_dat_har,
      action = "abort",
      call = call
    )
  }
  if (!inherits(args_list$par, "par")) {
    inferred_type <- attr(args_list$par, "data_type")
    .cli_action(data_err$invalid_par_har,
      action = "abort",
      call = call
    )
  }
  if (!inherits(args_list$set, "set")) {
    inferred_type <- attr(args_list$set, "data_type")
    .cli_action(dat_err$invalid_set_har,
      action = "abort",
      call = call
    )
  }

  args_list$metadata <- .get_metadata(args_list$dat_input)

  if (!is.null(args_list$target_format)) {
    if (purrr::pluck(args_list, "metadata", "data_format") %=% args_list$target_format) {
      .cli_action(
        data_err$invalid_convert,
        action = "abort",
        call = call
      )
    }
  }
  if (!is.null(args_list$time_steps)) {
    args_list$time_steps <- .check_time_steps(
      t0 = purrr::pluck(args_list, "metadata", "reference_year"),
      time_steps = args_list$time_steps,
      call = call
    )
  }

  return(args_list)
}