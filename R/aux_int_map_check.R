#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
.internal_map_check <- function(set,
                                database_version,
                                mapping) {

  # internal mappings
  # default data format
  data_format <- switch(EXPR = database_version,
                        "v9" = "v6.2",
                        "v10" = "v6.2",
                        "v11" = "v7.0")

  set_mappings <- purrr::pluck(.x = mappings, database_version, data_format, set)
  available_mappings <- colnames(x = set_mappings)[-1]

  if (!any(is.element(el = colnames(x = set_mappings), set = available_mappings))) {
    stop(
      paste(
        "The internal mapping chosen:",
        dQuote(x = mapping),
        "is not available.\n",
        "Available internal mappings for",
        set,
        "include:",
        paste(available_mappings, collapse = ", ")
      )
    )
  } else {
    set_ele <- as.list(x = unique(x = set_mappings[, ..mapping]))
    names(x = set_ele) <- set
  }
    return(set_ele)
}
