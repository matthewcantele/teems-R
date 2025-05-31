#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.check_external_set_map <- function(map_name,
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
  
  set_mapping <- read.csv(file = set_map)
  # check that required ele are present
  supplied_ele <- purrr::pluck(.x = set_mapping, 1)
  req_ele <- purrr::pluck(.x = mappings, metadata[["database_version"]], data_format, map_name, 1)
  
  if (!all(is.element(el = req_ele, set = supplied_ele))) {
    missing_ele <- req_ele[!is.element(el = req_ele, set = supplied_ele)]
    .cli_action(msg = "The set mapping for {.val {map_name}} is missing mappings
                for {.val {missing_ele}}.",
                action = "abort")
  }
  set_ele <- list(sort(x = unique(x = purrr::pluck(.x = set_mapping, 2))))
  names(x = set_ele) <- map_name
  attr(x = set_ele, which = "set_mapping_file") <- set_map

  return(set_ele)
}
