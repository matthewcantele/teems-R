.convert_id <- function(header,
                        table,
                        origin,
                        colname = FALSE) {
  target <- ifelse(test = identical(x = origin, y = "v6.2"),
                   yes = "v7.0",
                   no = "v6.2")

  id <- grep(pattern = header[["header_name"]], x = table[[paste0(origin, "header")]])
  new_name <- table[id, paste0(target, "header")]
  #new_descr <- table[id, paste0(target, "description")]

  if (!identical(x = new_name, y = character(0)) && !is.na(x = new_name)) {
    header[["header_name"]] <- new_name
    #header[["information"]] <- new_descr
    if (colname) {
      colnames(header[["dt"]]) <- new_name
    }
  }
  return(header)
}
