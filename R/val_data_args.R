#' @importFrom purrr pluck
#' @importFrom rlang arg_match
#' 
#' @noRd
#' @keywords internal
.validate_data_args <- function(a,
                                call) {
  
  if (length(a$set_mappings) %=% 0L) {
    .cli_action(load_err$missing_set_mappings,
                action = "abort",
                call = call
    )
  }
  
  checklist <- list(
    dat_input = "character",
    par_input = "character",
    set_input = "character",
    aux_input = c("NULL", "character"),
    unaggregated_input = c("NULL", "character", "list"),
    aggregated_input = c("NULL", "character", "list"),
    convert_format = "logical",
    time_steps = c("NULL", "numeric"),
    set_mappings = "list"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  a$dat_input <- .check_input(a$dat_input,
    valid_ext = "har",
    call = call
  )

  a$par_input <- .check_input(a$par_input,
    valid_ext = "har",
    call = call
  )

  a$set_input <- .check_input(a$set_input,
    valid_ext = "har",
    call = call
  )
  
  if (!is.null(a$aux_input)) {
    a$aux_input <- .check_input(a$aux_input,
                                valid_ext = "har",
                                call = call
    )
  }
  
  if (!is.null(a$unaggregated_input)) {
    a$unaggregated_input <- .prep_input_data(
      data = a$unaggregated_input,
      data_class = c("character", "data.frame"),
      call = call
    )

  }
  
  if (!is.null(a$aggregated_input)) {
    a$aggregated_input <- .prep_input_data(
      data = a$aggregated_input,
      data_class = c("character", "data.frame", "numeric"),
      call
    )

  }

  if (!is.null(a$target_format)) {
    target_format <- a$target_format
    a$target_format <- rlang::arg_match(
      arg = target_format,
      values = c("v6.2", "v7.0")
    )
    if (is.null(a$tab_file)) {
      .cli_action(data_err$missing_tar,
        action = "abort",
        call = call
      )
    }
  }

  if (!is.null(a$tab_file)) {
    a$tab_file <- .check_input(
      file = a$tab_file,
      valid_ext = "tab",
      call = call
    )
  }

  return(a)
}