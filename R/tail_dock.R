#' @keywords internal
#' @noRd
.dock_tail <- function(string,
                       length = 1) {
  docked_str <- substr(
    string,
    1,
    nchar(string) - length
  )

  return(docked_str)
}