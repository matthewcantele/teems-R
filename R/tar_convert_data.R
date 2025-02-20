#' @importFrom purrr map2 pluck
#' @importFrom data.table setnames
#' 
#' @keywords internal
#' @noRd
.convert_data <- function(ls_array,
                          data_format,
                          data_type,
                          coeff_extract = NULL) {

  ls_array <- .convert_format(
    ls_array = ls_array,
    data_format = data_format,
    data_type = data_type
  )
browser()
  if (!identical(x = data_type, y = "set")) {
    # we could do set info here but no point
    # use tablo extract for coefficient and descriptive data
    r_idx <- match(x = names(x = ls_array), table = coeff_extract[["header"]])
    ls_array <- purrr::map2(
      .x = ls_array,
      .y = r_idx,
      .f = function(dat, id) {
        if (!is.na(x = id)) {
          dat[["coefficient"]] <- purrr::pluck(coeff_extract[id, ], "name", 1)
          dat[["information"]] <- trimws(x = gsub(
            pattern = "#",
            replacement = "",
            x = purrr::pluck(coeff_extract[id, ], "information", 1)
          ))
        }
        return(dat)
      }
    )
  }
  return(ls_array)
}