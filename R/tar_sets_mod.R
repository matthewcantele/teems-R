.modify_sets <- function(set_array,
                         set_extract,
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

  r_idx <- match(x = names(x = set_array), table = set_extract[["header"]])
  set_array <- purrr::map2(
    .x = set_array,
    .y = r_idx,
    .f = function(h, id) {
      if (!is.na(x = id)) {
        label <- purrr::pluck(.x = set_extract, "label", id)
        purrr::pluck(.x = h, "label") <- label
      } else {
        purrr::pluck(.x = h, "label") <- NA
      }
      purrr::pluck(.x = h, "coefficient") <- NA
      return(h)
    }
  )

  return(set_array)
}