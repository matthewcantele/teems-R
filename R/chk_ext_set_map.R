#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.check_external_set_map <- function(map_name,
                                    metadata,
                                    set_map,
                                    call) {
  if (is.null(metadata$data_format)) {
    data_format <- switch(metadata$database_version,
      "v9" = "v6.2",
      "v10" = "v6.2",
      "v11" = "v7.0"
    )
  } else {
    data_format <- metadata$data_format
  }

  set_mapping <- read.csv(set_map)
  # check that required ele are present
  supplied_ele <- purrr::pluck(set_mapping, 1)
  req_ele <- purrr::pluck(mappings, metadata$database_version, data_format, map_name, 1)

  if (is.null(req_ele)) {
    .cli_action(load_err$no_ele_ext_mapping,
      action = "abort",
      call = call
    )
  }

  if (!all(req_ele %in% supplied_ele)) {
    missing_ele <- setdiff(req_ele, supplied_ele)
    .cli_action(load_err$missing_ele_ext_mapping,
      action = "abort",
      call = call
    )
  }
  browser()
  set_ele <- list(sort(unique(purrr::pluck(set_mapping, 2))))
  names(set_ele) <- map_name
  attr(set_ele, "set_mapping_file") <- set_map

  return(set_ele)
}
