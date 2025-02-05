#' @importFrom purrr map
#' @return A list containing the original Tablo data, the extracted components,
#'   and the file name.
#' @keywords internal
#' @noRd
.tablo_files <- function(parsed_tablo) {
  # File statements ############################################################
  files <- subset(x = parsed_tablo, subset = {
    is.element(
      el = tolower(x = type),
      set = "file"
    )
  })

  files[["names"]] <- trimws(x = purrr::map(sapply(X = files[["remainder"]], FUN = strsplit, split = " "), 1))
  browser()
  files[["remainder"]] <- .advance_remainder(
    type = "files",
    pattern = files[["names"]]
  )

  data.table::setnames(x = files,
                       old = "remainder",
                       new = "information")

  return(files)
}
