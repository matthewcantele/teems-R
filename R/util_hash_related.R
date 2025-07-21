#' @importFrom purrr map list_flatten compact
#' @noRd
#' @keywords internal
.extract_hash <- function(x) {
  if (is.null(x)) {
    return(NULL)
  } else {
    purrr::map(x, function(s) {
      id <- attr(s, "call_id")
      call <- attr(s, "call")
      if (!any(is.null(id), is.null(call))) {
        list(id = id, call = call)
      } else {
        return(NULL)
      }
    })
  }
}

.hash_table <- function(model_config,
                        load_config) {

  hash_tbl <- list(
    .extract_hash(model_config$shock),
    .extract_hash(model_config$swap_in),
    .extract_hash(model_config$swap_out),
    .extract_hash(list(load_config))
  )

  hash_tbl <- purrr::compact(purrr::list_flatten(purrr::compact(hash_tbl)))
  return(hash_tbl)
}
