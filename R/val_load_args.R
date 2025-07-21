#' @importFrom purrr pmap
#' @importFrom uuid UUIDgenerate
#' 
#' @keywords internal
.validate_load_args <- function(args_list,
                                call) {
  checklist <- list(
    ems_input = c("is.null", "is.character", "is.list"),
    dat_input = c("is.null", "is.character", "is.list"),
    par_input = c("is.null", "is.character", "is.list"),
    set_input = c("is.null", "is.character", "is.list"),
    aux_input = c("is.null", "is.character", "is.list"),
    unaggregated_input = c("is.null", "is.character", "is.list"),
    aggregated_input = c("is.null", "is.character", "is.list"),
    set_mappings = "is.list"
  )

  .check_arg_class(
    args_list = args_list,
    checklist = checklist,
    call = call
  )

  if (is.null(args_list$ems_input) && any(
    is.null(args_list$dat_input),
    is.null(args_list$par_input),
    is.null(args_list$set_input)
  )) {
    .cli_action(load_err$missing_ems_input,
      action = "abort",
      call = call
    )
  }

  if (!is.null(args_list$ems_input) && any(
    !is.null(args_list$dat_input),
    !is.null(args_list$par_input),
    !is.null(args_list$set_input)
  )) {
    .cli_action(load_err$extra_input,
      action = "warn",
      call = call
    )
  }

  if (length(args_list$set_mappings) %=% 0L) {
    .cli_action(load_err$missing_set_mappings,
      action = "abort",
      call = call
    )
  }

  call_id <- uuid::UUIDgenerate()
  if (!is.null(args_list$unaggregated_input)) {
    args_list$unaggregated_input <- .prep_input_data(
      data = args_list$unaggregated_input,
      data_class = c("character", "data.frame"),
      call = call
    )
    attr(args_list$unaggregated_input, "call_id") <- call_id
  }

  if (!is.null(args_list$aggregated_input)) {
    args_list$aggregated_input <- .prep_input_data(
      data = args_list$aggregated_input,
      data_class = c("character", "data.frame", "numeric"),
      call
    )
    attr(args_list$aggregated_input, "call_id") <- call_id
  }

  if (!is.null(args_list$dat_input)) {
    args_list$dat_input <- .check_input(
      file = args_list$dat_input,
      valid_ext = c("har", "qs2"),
      call = call
    )
    metadata <- .get_metadata(con = args_list$dat_input)
  } else {
    metadata <- attr(purrr::pluck(args_list, "ems_input", "dat"), "metadata")
    args_list$dat_input <- .check_input(
      file = purrr::pluck(args_list, "ems_input", "dat"),
      valid_ext = c("har", "qs2"),
      hash = attr(purrr::pluck(args_list, "ems_input", "dat"), "data_id"),
      call = call
    )
  }

  if (!is.null(args_list$par_input)) {
    args_list$par_input <- .check_input(
      file = args_list$par_input,
      valid_ext = c("har", "qs2"),
      call = call
    )
  } else {
    args_list$par_input <- .check_input(
      file = purrr::pluck(args_list, "ems_input", "par"),
      valid_ext = c("har", "qs2"),
      hash = attr(purrr::pluck(args_list, "ems_input", "par"), "data_id"),
      call = call
    )
  }

  if (!is.null(args_list$set_input)) {
    args_list$set_input <- .check_input(
      file = args_list$set_input,
      valid_ext = c("har", "qs2"),
      call = call
    )
  } else {
    args_list$set_input <- .check_input(
      file = purrr::pluck(args_list, "ems_input", "set"),
      valid_ext = c("har", "qs2"),
      hash = attr(purrr::pluck(args_list, "ems_input", "set"), "data_id"),
      call = call
    )
  }

  args_list$set_mappings <- .check_set_mappings(
    set_mappings = args_list$set_mappings,
    metadata = metadata,
    call = call
  )

  if (!is.null(args_list$aux_input)) {
    args_list[["aux_input"]] <- .check_input(
      file = args_list$aux_input,
      valid_ext = c("har", "qs2"),
      call = call
    )
  }

  .check_database_version(
    vetted = c("v9A", "v10A", "v11c"),
    provided = metadata$full_database_version,
    call = call
  )

  .inform_metadata(metadata = metadata)
  if (!is.null(args_list$ems_input)) {
    args_list <- args_list[!names(args_list) %in% "ems_input"]
  }

  args_list <- c(args_list, metadata = list(metadata))

  config <- structure(args_list,
    call_id = call_id,
    call = call
  )

  return(config)
}