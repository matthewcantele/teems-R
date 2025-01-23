#' @importFrom purrr pluck
#' @importFrom cli cli_abort
#'
#' @keywords internal
#' @noRd
.check_internal_set_mapping <- function(set,
                                        database_version,
                                        mapping,
                                        call) {
  data_format <- switch(
    EXPR = database_version,
    "v9" = "v6.2",
    "v10" = "v6.2",
    "v11" = "v7.0"
  )

  set_mappings <- purrr::pluck(.x = mappings, database_version, data_format, set)
  available_mappings <- colnames(x = set_mappings)[-1]

  if (!is.element(el = mapping, set = available_mappings)) {
    cli::cli_abort(
      c(
        "The internal mapping selected: {.val {mapping}}, for set: {set} does not exist.",
        "Available mappings for {set} include {.val {available_mappings}}"
      ),
      # add information for what the mappings are ??mappings
      call = call
    )
  } else {
    set_ele <- as.list(x = unique(x = set_mappings[, ..mapping]))
    names(x = set_ele) <- set
  }
  return(set_ele)
}
