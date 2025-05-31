#' @importFrom cli cli_inform cli_abort
#' @importFrom targets tar_config_set
#' @importFrom rlang call_args
#' 
#' @keywords internal
#' @noRd
.assert_config <- function(model_config,
                           set_config,
                           closure_config,
                           call,
                           store_dir,
                           base_dir,
                           model_name,
                           quiet) {
  if (is.null(x = closure_config) || is.null(x = closure_config[["closure_file"]])) {
      if (!quiet) {
        cli::cli_inform(c("i" = "No {.arg closure_file} has been provided so 
                          the standard model-specific closure will be used."))
    }
    tab_file <- rlang::call_args(model_config[["call"]])[["tab_file"]]
    closure <- .infer_closure(tab_file = tab_file)
    closure_file <- .teems_cache(
      input = closure,
      file = tab_file,
      ext = "cls",
      dir = "cls_files"
    )
    closure_config[["closure_file"]] <- closure_file
  }

return(closure_config)
}