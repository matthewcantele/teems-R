#' @keywords internal
#' @noRd
.convert_id <- function(header,
                        table,
                        target_format) {
  origin <- ifelse(target_format %=% "v6.2",
    "v7.0",
    "v6.2"
  )

  id <- grep(header$header, table[[paste0(origin, "header")]])
  new_name <- table[id, paste0(target_format, "header")]

  if (!new_name %=% character(0) && !is.na(new_name)) {
    header$header <- new_name
  }
  return(header)
}
