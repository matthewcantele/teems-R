#' @importFrom purrr map2
#' @importFrom tools file_ext
#' @importFrom cli cli_h1 cli_text
#'
#'
#' @keywords internal
#' @noRd
.check_set_mappings <- function(set_mappings,
                                metadata,
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
  
  available_mappings <- purrr::pluck(mappings, metadata$database_version, data_format)
  ls_set_ele <- purrr::map2(
    set_mappings,
    names(set_mappings),
    function(set_map, map_name) {
      if (tolower(tools::file_ext(set_map)) %=% "csv") {
        set_map <- .check_input(
          file = set_map,
          valid_ext = "csv",
          call = call
        )
        set_ele <- .check_external_set_map(
          map_name = map_name,
          metadata = metadata,
          set_map = set_map,
          call = call
        )
      } else {
        set_ele <- .check_internal_set_map(
          map_name = map_name,
          set_map = set_map,
          available_mappings = available_mappings,
          call = call
        )
      }
      return(set_ele)
    }
  )

  if (.o_verbose()) {
    cli::cli_h1("Core sets")
    purrr::map2(
      names(ls_set_ele),
      ls_set_ele,
      function(set_name, ele) {
        ele <- sort(unlist(unique(ele[,2])))
        cli::cli_text("{set_name}: {.val {ele}}")
      }
    )
  }

  return(ls_set_ele)
}