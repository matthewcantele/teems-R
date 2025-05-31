#' @keywords internal
#' @noRd
.convert_id <- function(header,
                        table,
                        target_format) {

  origin <- ifelse(test = identical(x = target_format, y = "v6.2"),
                   yes = "v7.0",
                   no = "v6.2")
  
  id <- grep(pattern = header[["header"]], x = table[[paste0(origin, "header")]])
  new_name <- table[id, paste0(target_format, "header")]
  
  if (!identical(x = new_name, y = character(0)) && !is.na(x = new_name)) {
    header[["header"]] <- new_name
  }
  return(header)
}
