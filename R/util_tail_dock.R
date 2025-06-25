#' @keywords internal
#' @noRd
.dock_tail <- function(string,
                       length = 1) {
  docked_str <- substr(
    x = string,
    start = 1,
    stop = nchar(x = string) - length
  )

  return(docked_str)
}
