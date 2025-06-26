#' @importFrom purrr pmap
#' 
#' @keywords internal
.validate_data_args <- function(args_list,
                                call,
                                quiet) {
  checklist <- list(
    dat_input = c("is.character", "is.list"),
    par_input = c("is.character", "is.list"),
    aux_input = c("is.null", "is.character", "is.list"),
    aggregated_data = list(
      c("is.null", "is.character", "is.list", "is.data.frame"),
      c("is.character", "is.data.frame")
    ),
    unaggregated_data = list(
      c("is.null", "is.character", "is.list", "is.data.frame"),
      c("is.character", "is.data.frame")
    ),
    quiet = "is.logical"
  )

  .check_arg_class(args_list = args_list,
                   checklist = checklist,
                   call = call)
  browser()
}