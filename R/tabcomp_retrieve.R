#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#'
#' @keywords internal
#' @noRd
.retrieve_tab_comp <- function(tab_path,
                               which,
                               call) {
  tab <- .check_tab_file(
    tab_file = tab_path,
    call = call
  )
  extract <- tibble::tibble(
    type = purrr::map_chr(strsplit(tab, split = " ", perl = TRUE), 1),
    remainder = sub("^\\S+\\s*", "", tab)
  )

  extract$row_id <- seq(1, nrow(extract))
  if (which %=% "variable") {
    comp_extract <- .parse_tab_obj(
      extract = extract,
      obj_type = "variable",
      call = call
    )
  } else if (which %=% "coefficient") {
    comp_extract <- .parse_tab_obj(
      extract = extract,
      obj_type = "coefficient",
      call = call
    )
  }

  file_state <- subset(
    extract,
    type == "File",
    remainder,
    1
  )
  input_files <- trimws(purrr::map_chr(strsplit(
    file_state[!grepl("\\(new\\)", file_state)],
    "#"
  ), 1))

  input_files <- file.path(dirname(tab_path), paste0(input_files, ".txt"))
  attr(comp_extract, "input_files") <- input_files
  return(comp_extract)
}