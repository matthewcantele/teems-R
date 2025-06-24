#' @importFrom rlang arg_match
#' 
#' @keywords internal
.validate_shock_args <- function(args_list,
                                 call) {
 
  checklist <- list(
    var = "is.character",
    type = "is.character",
    value = c("is.null", "is.numeric", "is.data.frame"),
    file = c("is.null", "is.character"),
    set = c("is.null", "is.character"),
    ele = c("is.list", "is.null"),
    check_status = "is.logical"
  )
  .check_arg_class(args_list = args_list,
                   checklist = checklist,
                   call = call)
  
  type <- args_list[["type"]]
  rlang::arg_match(arg = type,
                   values = c("uniform", "custom", "scenario"),
                   error_call = call)

  if (identical(x = args_list[["type"]], y = "uniform")) {
    if (is.null(x = args_list[["value"]]) || !is.numeric(x = args_list[["value"]])) {
      .cli_action(msg = "{.arg value} must be provided a numeric value for all 
                  {.arg uniform} shocks.",
                  action = "abort",
                  call = call)
    }
  }

  if (is.element(el = args_list[["type"]], set = c("custom", "scenario"))) {
    if (is.null(x = args_list[["file"]]) && is.null(x = args_list[["value"]])) {
      .cli_action(msg = "Either {.arg file} or {.arg value} must be provided 
                  for {.val type} shocks.",
                  action = "abort",
                  call = call)
    }
    
    if (!is.null(x = args_list[["file"]])) {
      args_list[["file"]] <- .check_input(
        file = args_list[["file"]],
        valid_ext = "csv",
        call = call
      )
    }
    
    if (!is.null(x = args_list[["value"]]) && !is.data.frame(x = args_list[["Value"]])) {
      .cli_action(msg = "The {.arg value} provided for a {.val type} shock must 
                  be a data frame.")
    }
  }
  
  return(args_list)
}