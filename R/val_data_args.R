#' @importFrom purrr pmap
#' @importFrom uuid UUIDgenerate
#' 
#' @keywords internal
.validate_data_args <- function(args_list,
                                call) {

  checklist <- list(
    dat_input = c("is.character", "is.list"),
    par_input = c("is.character", "is.list"),
    aux_input = c("is.null", "is.character", "is.list"),
    unaggregated_input =  c("is.null", "is.character", "is.list"),
    aggregated_input =  c("is.null", "is.character", "is.list")
  )

  .check_arg_class(
    args_list = args_list,
    checklist = checklist,
    call = call
  )

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

  args_list$dat_input <- .check_input(
    file = args_list$dat_input,
    valid_ext = c("har", "qs2"),
    call = call
  )
  
  args_list$par_input <- .check_input(
    file = args_list$par_input,
    valid_ext = c("har", "qs2"),
    call = call
  )

  if (!is.null(args_list$aux_input)) {
    args_list[["aux_input"]] <- .check_input(
      file = args_list$aux_input,
      valid_ext = c("har", "qs2"),
      call = call
    )
  }
  
  metadata <- .get_metadata(con = args_list$dat_input)
  
  .check_database_version(
    vetted = c("v9A", "v10A", "v11c"),
    provided = metadata$full_database_version,
    call = call
  )
  
  .inform_metadata(metadata = metadata)
  
  args_list <- c(args_list, metadata = list(metadata))
  
  config <- structure(args_list,
    call_id = call_id,
    call = call
  )

  return(config)
}