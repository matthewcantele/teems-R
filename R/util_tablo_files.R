#' @importFrom purrr map
#' @importFrom data.table setnames
#'
#' @keywords internal
#' @noRd
.tablo_files <- function(tab_extract,
                         call) {

  files <- subset(tab_extract, tolower(type) %in% "file")

  files[["names"]] <- trimws(purrr::map_chr(
    purrr::map(purrr::map(files$remainder, strsplit, " "), 1), 1))

  files[["remainder"]] <- .advance_remainder(
    remainder = files[["remainder"]],
    pattern = files[["names"]]
  )

  data.table::setnames(
    x = files,
    old = "remainder",
    new = "label"
  )

  return(files)
}