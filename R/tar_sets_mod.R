.modify_sets <- function(set_array,
                         full_exclude,
                         append = NULL,
                         call) {

  if (!is.null(x = full_exclude)) {
    set_array <- set_array[!is.element(el = names(x = set_array), set = full_exclude)]
  }
  
  if (is.null(x = set_array)) {
    set_array <- append
  } else if (!is.null(x = set_array) && !is.null(x = append)) {
    set_array <- c(set_array, append)
  }

  set_array <- lapply(
    X = set_array,
    FUN = function(header) {
        header[["data"]] <- tolower(x = gsub(
          pattern = "CGDS",
          replacement = "zcgds",
          x = header[["data"]],
          ignore.case = TRUE
        ))
      return(header)
    }
  )
  
  set_array <- lapply(X = set_array,
                      FUN = function(header) {
                        header[["coefficient"]] <- NA
                        return(header)
                      })
  return(set_array)
}