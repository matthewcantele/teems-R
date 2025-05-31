#' @importFrom purrr pluck
#'
#' @keywords internal
#' @noRd
.check_internal_set_map <- function(map_name,
                                    metadata,
                                    set_map,
                                    call) {

  if (is.null(metadata[["data_format"]])) {
    data_format <- switch(
      EXPR = metadata[["database_version"]],
      "v9" = "v6.2",
      "v10" = "v6.2",
      "v11" = "v7.0"
    )
  } else {
    data_format <- metadata[["data_format"]]
  }

  set_mappings <- purrr::pluck(.x = mappings, metadata[["database_version"]], data_format, map_name)
  available_mappings <- colnames(x = set_mappings)[-1]

  if (!is.element(el = set_map, set = available_mappings)) {
    .cli_action(
      msg = c("The internal mapping selected: {.val {set_map}},
    for set {.val {map_name}} does not exist.", "Available internal mappings
    for {.val {map_name}} include {.val {available_mappings}}"),
      action = c("abort", "inform"),
      call = call
    )
    # add information for what the mappings are ??mappings
  } else {
    sel_mapping <- set_mappings[, mget(x = c(colnames(x = set_mappings)[1], ..set_map))]
    file <- paste(metadata[["database_version"]], map_name, set_map, sep = "_")
    set_file <- .teems_cache(
      input = sel_mapping,
      file = file,
      ext = "csv",
      dir = "mappings"
    )
    set_ele <- as.list(x = unique(x = set_mappings[, ..set_map]))
    attr(x = set_ele, which = "set_mapping_file") <- set_file
  }
  return(set_ele)
}