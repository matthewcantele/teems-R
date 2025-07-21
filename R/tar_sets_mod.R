#' @importFrom purrr map2 pluck
#' 
#' @keywords internal
#' @noRd
.modify_sets <- function(set_array,
                         set_extract,
                         append = NULL,
                         call) {

  if (!is.null(.o_full_exclude())) {
    set_array <- set_array[!names(set_array) %in% .o_full_exclude()]
  }

  if (is.null(set_array)) {
    set_array <- append
  } else if (!is.null(set_array) && !is.null(append)) {
    set_array <- c(set_array, append)
  }
  
  set_array <- lapply(
    set_array,
    FUN = function(header) {
      if (is.character(header$data)) {
        header$data <- tolower(gsub(
          "CGDS",
          "zcgds",
          header$data,
          ignore.case = TRUE
        ))
      }
      return(header)
    }
  )

  r_idx_header <- match(names(set_array), set_extract$header)
  r_idx_name <- match(names(set_array), set_extract$name)

  set_array <- purrr::pmap(
    list(
      set_array,
      r_idx_header,
      r_idx_name
    ),
    function(arr, h_id, n_id) {
      if (!is.na(h_id)) {
        label <- purrr::pluck(set_extract, "label", h_id)
        purrr::pluck(arr, "label") <- label
      }

      if (is.na(h_id) && !is.na(n_id)) {
        label <- purrr::pluck(set_extract, "label", n_id)
        purrr::pluck(arr, "label") <- label
      }

      if (is.na(h_id) && is.na(n_id)) {
        purrr::pluck(arr, "label") <- NA
      }
      
      purrr::pluck(arr, "coefficient") <- NA
      return(arr)
    }
  )
  
  class(set_array) <- "set"

  return(set_array)
}