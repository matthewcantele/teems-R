#' @importFrom rlang current_env
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.update_set_names <- function(ls_array,
                              coeff_extract,
                              metadata,
                              data_type) {
  list2env(x = metadata, envir = rlang::current_env())

  ls_array <- lapply(
    X = ls_array,
    FUN = function(header) {
      coeff <- header[["coefficient"]]
      mixed_names <- purrr::pluck(coeff_extract, "ls_mixed_idx", coeff)
      if (!identical(x = mixed_names, y = "ALLTIMEt")) {
        mixed_names <- setdiff(x = mixed_names, y = "ALLTIMEt")
      }
      stnd_names <- names(x = dimnames(x = header[["data"]]))
      # standard NULL if no-set var, mixed null if not in in extract
      if (!is.null(x = stnd_names) && !is.null(x = mixed_names)) {
        names(x = dimnames(x = header[["data"]])) <- mixed_names
      } else if (!is.null(x = stnd_names) && is.null(x = mixed_names)) {
        if (identical(
          x = sum(grepl(pattern = "REG", x = stnd_names),
            na.rm = TRUE
          ),
          y = 2L
        )) {
          stnd_names[duplicated(x = stnd_names)] <- "REGs"
          mixed_names <- sub(
            pattern = "^REG$",
            replacement = "REGr",
            x = stnd_names
          )
        } else if (identical(
          x = sum(
            grepl(
              pattern = "REG",
              x = stnd_names
            ),
            na.rm = TRUE
          ),
          y = 1L
        )) {
          mixed_names <- sub(
            pattern = "^REG$",
            replacement = "REGr",
            x = stnd_names
          )
        }
        origin_col <- paste(data_format, "upper", sep = "_")
        dest_col <- paste(model_version, "mixed", sep = "_")
        vect_mapping <- setNames(
          object = set_table[[dest_col]],
          nm = set_table[[origin_col]]
        )

        mixed_names <- ifelse(test = is.element(el = stnd_names, set = names(x = vect_mapping)),
          yes = vect_mapping[stnd_names],
          no = mixed_names
        )

        names(x = dimnames(x = header[["data"]])) <- mixed_names
      }
      return(header)
    }
  )

  return(ls_array)
}