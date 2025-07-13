#' @importFrom rlang current_env
#' 
#' @keywords internal
#' @noRd
.val_cst_scen_shk <- function(args_list,
                              call) {
  checklist <- list(
    var = "is.character",
    type = "is.character",
    input = c("is.character", "is.data.frame")
  )
  .check_arg_class(
    args_list = args_list,
    checklist = checklist,
    call = call
  )

  list2env(args_list, rlang::current_env())
  if (inherits(input, "character")) {
    args_list$input <- .check_input(
      file = input,
      valid_ext = "csv",
      call = call
    )
    value <- read.csv(args_list$input)
    value_colnames <- colnames(value)
  } else if (inherits(input, "data.frame")) {
    value_colnames <- colnames(input)
  }

  if (!"Value" %=% value_colnames[length(value_colnames)]) {
    if (inherits(input, "character")) {
      .cli_action(
        shk_err$cst_scen_val_file,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    } else if (inherits(input, "data.frame")) {
      .cli_action(
        shk_err$cst_scen_val_df,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    }
  }

  if (args_list$type %=% "scenario" && !"Year" %in% value_colnames) {
    if (inherits(input, "character")) {
      .cli_action(
        shk_err$scen_year_file,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    } else if (inherits(input, "data.frame")) {
      .cli_action(
        shk_err$scen_year_df,
        action = "abort",
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    }
  }

  if (inherits(input, "data.frame")) {
    file <- paste(var, paste0(format(Sys.time(), "%S"), sample(1:99, 1)), collapse = "_")
    args_list$input <- .teems_cache(
      input = input,
      file = file,
      ext = "csv"
    )
  }

  args_list$set <- setdiff(value_colnames, "Value")
  args_list <- structure(args_list,
    call_id = uuid::UUIDgenerate(),
    call = call,
    shock = TRUE,
    class = c(args_list$type, "list")
  )
  args_list <- list(args_list)
  return(args_list)
}