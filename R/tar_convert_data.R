#' @importFrom purrr map2 pluck
#' @importFrom data.table setnames
#' 
#' @keywords internal
#' @noRd
.convert_data <- function(data,
                          data_format) {
  browser()
  data_type <- attr(x = ls_array, "data_type")
  ls_array <- .convert_format(
    data = ls_array,
    data_format = metadata[["data_format"]],
    data_type = data_type
  )

  if (!identical(x = data_type, y = "set")) {
    # use tablo extract for coefficient, descriptive data, and mixed set names
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

          extract_col <- purrr::pluck(coeff_extract[id, ], "ls_mixed_idx", 1)
          if (!identical(x = extract_col, y = "null_set")) {
            existing_col <- colnames(x = dat[["dt"]][, !"Value"])
            if (!identical(x = existing_col, y = extract_col)) {
              data.table::setnames(
                x = dat[["dt"]],
                old = existing_col,
                new = extract_col
              )
            }
          }
        }
        return(dat)
      }
    )
  }

  attr(x = ls_array, which = "data_type") <- data_type
}