#' @importFrom purrr map2 pluck
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

  if (!identical(x = data_type, y = "set")) {
    # we could do set info here but no point
    # use tablo extract for coefficient and descriptive data
    r_idx <- match(x = names(x = ls_array), table = coeff_extract[["header"]])
    ls_array <- purrr::map2(
      .x = ls_array,
      .y = r_idx,
      .f = function(header, id) {
        if (!is.na(x = id)) {
          header[["coefficient"]] <- purrr::pluck(coeff_extract[id, ], "name", 1)
          header[["information"]] <- trimws(x = gsub(
            pattern = "#",
            replacement = "",
            x = purrr::pluck(coeff_extract[id, ], "information", 1)
          ))
        } else {
          header[["coefficient"]] <- header[["header_name"]]
          header[["information"]] <- NA
        }

        # missing aggregate info (TRUE if missing)
        if (!is.element(el = "aggregate", set = names(x = header))) {
          header[["aggregate"]] <- TRUE
        }
        
        # missing type info (real if missing)
        if (!is.element(el = "type", set = names(x = header))) {
          header[["type"]] <- "real"
        }

        return(header)
      }
    )
  }
  return(ls_array)
}