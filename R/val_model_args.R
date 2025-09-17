#' @importFrom cli cli_verbatim
#' 
#' @keywords internal
#' @noRd
.validate_model_args <- function(a,
                                 call) {
  checklist <- list(
    tab_file = "character",
    var_omit = c("NULL", "character")

  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  a$tab_file <- .check_input(
    file = a$tab_file,
    valid_ext = "tab",
    call = call
  )

  return(a)
}