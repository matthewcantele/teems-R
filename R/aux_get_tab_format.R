#' .parse_tablo function
#'
#' This function parses a Tablo file (.tab), extracting key components and
#' optionally exporting the parsed data. It handles comments, retains key
#' statements, and processes the data to identify types, qualifiers, and other
#' attributes.
#'
#' @param tab_file The path to the Tablo file to be parsed.
#'
#' @importFrom tibble tibble
#' @importFrom purrr map2
#' @return A list containing the original Tablo data, the extracted components,
#'   and the file name.
#' @keywords internal
#' @noRd
.get_tab_format <- function(tab_file) {
  if (grepl(pattern = "\\.tab", tab_file)) {
      tab <- readChar(
        con = tab_file,
        nchars = file.info(tab_file)[["size"]]
      )
  } else {
    tab <- internal_tab[[tab_file]]
    tab_file <- paste0(tab_file, ".tab")
  }

  # get tab data format (first 200 char)
  tab_preface <- substring(text = tab, first = 3, last = 200)
  # extract the line containing the version number
  lines <- unlist(strsplit(tab_preface, "\n"))
  version_line <- grep(pattern = "Version", x = lines, value = TRUE)
  model_version <- sub(pattern = ".*Version ([0-9]+\\.[0-9]+).*",
                     replacement = "\\1",
                     x = version_line)

  final_format <- paste0("v", model_version)
  return(final_format)
}
