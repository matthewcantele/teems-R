#' @importFrom purrr pluck
#' @importFrom targets tar_make tar_errored tar_meta tar_load_everything
#' 
#' @keywords internal
#' @noRd
.execute_pipeline <- function(teems_paths,
                              call_hash_tbl,
                              #tar_load_everything,
                              .testing) {
  if (!.testing) {
    output <- try({
      targets::tar_make(
        script = teems_paths[["pipeline"]],
        store = teems_paths[["store"]])
    }, silent = TRUE)
    if (inherits(x = output, what = "try-error")) {
      browser()
      errored_tar <- targets::tar_errored(store = teems_paths[["store"]])
      raw_error <- targets::tar_meta(names = errored_tar,
                                     store = teems_paths[["store"]])[["error"]]

      call_id <- purrr::pluck(strsplit(raw_error, ";"), 1, 4)
      call <- purrr::pluck(call_hash_tbl, match(call_id, map_chr(call_hash_tbl, 1)), "call")
      error_call_body <- head(strsplit(raw_error, ";")[[1]], -1)
      # first_colon <- regexpr(pattern = "(?<!:):(?!:)",
      #                        text =  raw_error,
      #                        perl = TRUE)
      # error_inputs <- trimws(x = substring(text = raw_error,
      #                                      first = first_colon + 1))
      
      # drop purrr baggage
      # if (grepl(pattern = ": ! ", x = error_call_body[[1]])) {
      #   error_call_body[[1]] <- strsplit(x = error_call_body[[1]], split = ": ! ")[[1]][2]
      # }
      expr <- parse(text = error_call_body)
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