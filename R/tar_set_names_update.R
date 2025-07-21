#' @importFrom rlang current_env
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.update_set_names <- function(ls_array,
                              coeff_extract,
                              data_format) {

  r_idx <- match(names(ls_array), coeff_extract$header)
  new_names <- coeff_extract$ls_mixed_idx[r_idx]
  stnd_names <- purrr::map(ls_array, function(x) {
    names(dimnames(x$data))
  })

  ls_array <- purrr::pmap(
    list(
      ls_array,
      new_names,
      stnd_names
    ),
    function(header, new_names, stnd_names) {
      # have to use header here for matching because older database has
      # coefficient names in input data that no longer exist in input data
      # but are present in the model (e.g., PRIVEXP)
      if ((is.null(new_names) && is.null(stnd_names)) || new_names %=% "null_set") {
        return(header)
      }

      if (is.null(new_names)) {
        if (sum(grepl("REG", stnd_names), na.rm = TRUE) %=% 2L) {
          new_names[duplicated(stnd_names)] <- "REGs"
          new_names <- sub("^REG$", "REGr", stnd_names)
        } else if (sum(grepl("REG", stnd_names), na.rm = TRUE) %=% 1L) {
          new_names <- sub("^REG$", "REGr", stnd_names)
        }

        origin_col <- paste(data_format, "upper", sep = "_")
        dest_col <- paste(data_format, "mixed", sep = "_")
        vect_mapping <- setNames(set_table[[dest_col]], set_table[[origin_col]])

        new_names <- ifelse(new_names %in% names(vect_mapping),
          vect_mapping[new_names],
          new_names
        )
      }
      names(dimnames(header$data)) <- new_names
      return(header)
    }
  )

  class(ls_array) <- "coefficient"
  return(ls_array)
}