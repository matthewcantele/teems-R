#' @importFrom purrr list_flatten
#' @keywords internal
#' @noRd
.internal_map_check <- function(set,
                                database_version,
                                mapping) {

  # internal mappings
  internal_mappings <- subset(x = mappings,
                              subset = {is.element(el = names(x = mappings),
                                                   set = database_version)})[[1]]

  set_mappings <- internal_mappings[is.element(el = names(x = internal_mappings), set = set)][[1]]
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
