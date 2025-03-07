#' @importFrom purrr map
#' @importFrom data.table setnames
#'
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

  files[["names"]] <- trimws(x = purrr::map(sapply(X = files[["remainder"]],
                                                   FUN = strsplit, split = " "), 1))

  files[["remainder"]] <- .advance_remainder(
    remainder = files[["remainder"]],
    pattern = files[["names"]]
  )

  data.table::setnames(
    x = files,
    old = "remainder",
    new = "information"
  )

  return(files)
}