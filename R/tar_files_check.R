#' Check Files
#'
#' This function checks if each Tablo File statement is named.
#'
#' @param file_inputs A named list of file inputs.
#' @param tab_file_inputs A data frame containing Tablo File statements.
#'
#' @return A named list of file inputs that were checked.
#' @keywords internal
#' @noRd
.check_files <- function(file_inputs,
                         tab_file_inputs) {

  # Validate inputs
  if (!is.list(file_inputs)) {
    stop(paste(dQuote("file_inputs"), "must be a list."))
  }

  if (!is.data.frame(tab_file_inputs)) {
    stop(paste(dQuote("tab_extract"), "must be a data frame."))
  }

  file_names <- tab_file_inputs[["names"]]
  # check that each Tablo File statement is named
  missing_names <- file_names[!is.element(el = file_names, set = names(x = file_inputs))]

  if (length(x = missing_names) > 0) {
    warning(paste0(
      "The following File statements within the Tab file have not been provided a name: ",
      paste(missing_names, collapse = ", "), "\n"
    ))
  }

  return(file_inputs)
}
