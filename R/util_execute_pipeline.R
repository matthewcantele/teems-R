#' @importFrom targets tar_make tar_errored tar_meta tar_load_everything
#' 
#' @keywords internal
#' @noRd
.execute_pipeline <- function(teems_paths,
                              base_call,
                              par_call,
                              set_call,
                              tar_load_everything,
                              .testing) {
  if (!.testing) {
    output <- try({
      targets::tar_make(
        script = teems_paths[["pipeline"]],
        store = teems_paths[["store"]])
    }, silent = TRUE)
    if (inherits(x = output, what = "try-error")) {
      errored_tar <- targets::tar_errored(store = teems_paths[["store"]])
      raw_error <- targets::tar_meta(names = errored_tar,
                                     store = teems_paths[["store"]])[["error"]]
      first_colon <- regexpr("(?<!:):(?!:)", raw_error, perl = TRUE)
      error_inputs <- trimws(x = substring(raw_error, first_colon + 1))
      expr <- parse(text = error_inputs)
      eval(expr = expr)
    }
  } else {
    targets::tar_make(
      script = teems_paths[["pipeline"]],
      callr_function = NULL,
      store = teems_paths[["store"]])
  }

return(invisible(NULL))
}