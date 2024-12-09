#' Create a New Header
#'
#' This function creates a new header by converting inputs into a single row
#' tibble. It allows for dynamic naming of columns based on the `header`
#' argument.
#'
#' @param ... Additional data classes to be included in the header.
#' @param header A character vector specifying the name of the the new header.
#' @param information A information for the header, included as a column in the
#'   resulting tibble.
#' @param dt Header numeric data.
#'
#' @return A single row tibble with columns named according to provided inputs
#'   including any additional arguments passed through `...`.
#' @importFrom tibble as_tibble
#' @keywords internal
#' @noRd
.create_header <- function(...,
                         header,
                         information,
                         dt) {
  # convert to single row tibble
  new_header <- tibble::as_tibble(x = c(mget(x = ls(sorted = FALSE)), list(...)))

  names(x = new_header[["dt"]]) <- new_header[["header"]]
  names(x = new_header[["information"]]) <- new_header[["header"]]

  if (any(x = colnames(new_header) == "v_class")) {
    names(new_header[["v_class"]]) <- new_header[["header"]]
  }

  return(new_header)
}
