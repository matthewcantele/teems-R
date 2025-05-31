#' @importFrom purrr map2
#' @importFrom tools file_ext
#' @importFrom cli cli_h1 cli_text
#'
#'
#' @keywords internal
#' @noRd
.check_set_mappings <- function(set_mappings,
                                time_format,
                                metadata,
                                call,
                                envir,
                                quiet) {
  ls_set_ele <- purrr::map2(
    .x = set_mappings,
    .y = names(x = set_mappings),
    .f = function(set_map, map_name) {
      if (identical(x = tolower(x = tools::file_ext(x = set_map)), y = "csv")) {
        set_map <- .check_input(
          file = set_map,
          valid_ext = "csv",
          call = call,
          internal = FALSE
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
          metadata = metadata,
          set_map = set_map,
          call = call
        )
      }
      return(set_ele)
    }
  )
  
  if (!quiet) {
    cli::cli_h1(text = "Core sets")
    purrr::map2(
      .x = names(x = ls_set_ele),
      .y = ls_set_ele,
      .f = function(set_name, ele) {
        ele <- unlist(x = ele)
        cli::cli_text("{set_name}: {.val {ele}}")
      }
    )
  }

  return(ls_set_ele)
}