#' @importFrom purrr compact
#'
#' @keywords internal
#' @noRd
.get_setmap_info <- function(config) {
  set_map_files <- purrr::compact(.x = sapply(X = config,
                                              FUN = attr,
                                              which = "set_mapping_file"))
  set_map_files <- unlist(x = set_map_files)
  return(set_map_files)
}