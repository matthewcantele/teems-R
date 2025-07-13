#' @importFrom purrr map list_flatten compact
#' @noRd
#' @keywords internal
.extract_hash <- function(x) {
  if (is.null(x)) {return(NULL)}
  purrr::map(x, function(s) {
    list(id = attr(s, "call_id"), call = attr(s, "call"))
  })
}

.hash_table <- function(model_config,
                        data_config) {

  hash_tbl <- list(
    .extract_hash(model_config$shock),
    .extract_hash(model_config$swap_in),
    .extract_hash(model_config$swap_out),
    .extract_hash(list(data_config))
  )

  hash_tbl <- purrr::list_flatten(purrr::compact(hash_tbl))
  return(hash_tbl)
}