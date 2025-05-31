#' @importFrom rlang current_env
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.update_set_names <- function(ls_array,
                              coeff_extract,
                              data_format,
                              set_extract) {
  if (any(set_extract[["intertemporal"]])) {
    int_sets <- subset(x = set_extract,
                       subset = intertemporal,
                       select = name)[[1]]
    int_sets <- paste0(int_sets, "t")
    intertemporal <- TRUE
  } else {
    intertemporal <- FALSE
  }

  ls_array <- lapply(
    X = ls_array,
    FUN = function(header) {
      # have to use header here for matching because older database has
      # coefficient names in input data that no longer exist in input data
      # but are present in the model (e.g., PRIVEXP)
      r_idx <- match(x = header[["header"]], table = coeff_extract[["header"]])
      new_names <- purrr::pluck(.x = coeff_extract, "ls_mixed_idx", r_idx)
      stnd_names <- names(x = dimnames(x = header[["data"]]))
      if (is.null(x = new_names) && is.null(x = stnd_names)) {
        return(header)
      } 
      if (!is.null(x = new_names)) {
        if (!identical(x = new_names, y = "null_set")) {
          if (intertemporal) {
            if (any(is.element(el = int_sets, set = new_names))) {
              if (!identical(x = length(x = new_names), y = 1L)) {
              new_names <- setdiff(x = new_names, int_sets)
              } else {
                return(header)
              }
            }
          }
        } else {
          return(header)
        }
      } else {
        if (identical(
            x = sum(grepl(pattern = "REG", x = stnd_names),
              na.rm = TRUE
            ),
            y = 2L
          )) {
            new_names[duplicated(x = stnd_names)] <- "REGs"
            new_names <- sub(
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
            new_names <- sub(
              pattern = "^REG$",
              replacement = "REGr",
              x = stnd_names
            )
          }

          origin_col <- paste(data_format, "upper", sep = "_")
          dest_col <- paste(data_format, "mixed", sep = "_")
          vect_mapping <- setNames(
            object = set_table[[dest_col]],
            nm = set_table[[origin_col]]
          )

          new_names <- ifelse(test = is.element(el = new_names, set = names(x = vect_mapping)),
            yes = vect_mapping[new_names],
            no = new_names
          )
    }
          names(x = dimnames(x = header[["data"]])) <- new_names
      return(header)
    }
  )

  return(ls_array)
}