#' Convert arrays into CSVs and associated metadata
#'
#' This function takes a list of arrays and converts them into structured data
#' and related metadata. It also performs various transformations based on the
#' provided parameters.
#'
#' @inheritParams teems_model
#' @inheritParams .extract_metadata
#'
#' @param ls_array An array object produced by \code{.read_har()}
#' @param metadata A character vector specifying the database version, reference
#'   year, and data_format produced by \code{.read_har()} and extracted with
#'   \code{.extract_metadata()}.
#' @param coeff_extract Tablo extracted coefficient information.
#' @param time_sets Intertemporal-specific sets information.
#' @param lowercase A logical value indicating whether to convert all elements
#'   to lowercase. Default is TRUE.
#' @param zCGDS A logical value indicating whether to convert 'CGDS' to 'zCGDS'
#'   for sorting purposes. Default is TRUE.
#' @param set_array An set-specific array object produced by \code{.read_har()}
#'
#' @importFrom data.table as.data.table setnames
#' @importFrom purrr pluck
#' @return A list of data.tables for each header and metadata file for each data
#'   type.
#' @keywords internal
#' @noRd
.modify_data <- function(ls_array,
                         metadata,
                         coeff_extract,
                         time_sets,
                         full_exclude,
                         aux_data,
                         lowercase = TRUE,
                         zCGDS = TRUE,
                         set_array = NULL) {
  data_type <- attr(x = ls_array, "data_type")

  if (!zCGDS) {
    warning('Data.table C-Locale sorting (which is used in associated functions) will lead to errors with capital case "CGDS". This element is found in v6.2 data format.')
  }

  # if tab file format data format mismatch convert data
  if (!identical(x = metadata[["data_format"]],
                 y = metadata[["model_version"]])) {
    ls_array <- .convert_data_format(data = ls_array,
                                     data_format = metadata[["data_format"]],
                                     data_type = data_type)
    # use tablo extract for coefficient and descriptive data
    r_idx <- match(x = names(x = ls_array),
                   table = aux_data[["header"]])

    ls_array <- purrr::map2(.x = ls_array,
                .y = r_idx,
                .f = function(dat, id) {
                  dat[["coefficient"]] <- purrr::pluck(aux_data[id,], "name", 1)
                  dat[["information"]] <-  trimws(x = gsub(pattern = "#",
                                                           replacement = "",
                                                           x = purrr::pluck(aux_data[id,],
                                                                            "information",
                                                                            1)))
                  return(dat)
                })
  }

  # optional transformations
  # all set elements to lowercase
  if (lowercase) {
    ls_array <- lapply(X = ls_array, FUN = function(header) {
      # all set elements to lowercase except H1L (region names)
      if (!is.element(el = header[["header_name"]], set = c("H1L", "LREG"))) {
        col_names <- colnames(x = header[["dt"]])

        header[["dt"]] <- header[["dt"]][, lapply(X = .SD, FUN = function(char_col) {
          if (!is.numeric(x = char_col)) {
            char_col <- tolower(x = char_col)
          }
          return(char_col)
        }), .SDcols = col_names]
      }
      return(header)
    })
  }

  # 'CGDS/cgds' to 'zCGDS' (see arguments)
  if (zCGDS && identical(x = metadata[["model_version"]], y = "v6.2")) {
    ls_array <- lapply(X = ls_array, FUN = function(header) {
      col_names <- colnames(x = header[["dt"]])

      header[["dt"]] <- header[["dt"]][, lapply(X = .SD, FUN = function(char_col) {
        if (class(x = char_col) != "numeric") {
          char_col <-
            gsub(
              pattern = "cgds",
              replacement = "zCGDS",
              x = char_col,
              ignore.case = TRUE,
              perl = TRUE
            )
        }
        return(char_col)
      }), .SDcols = col_names]
      return(header)
    })
  }

  # remove unnecessary components (array, binary data) and add data_type
  ls_array <- lapply(X = ls_array, FUN = function(h) {
    h <- h[is.element(el = names(h), set = c("header_name", "coefficient", "information", "dt"))]
    return(h)
  })

  attr(x = ls_array, which = "data_type") <- data_type
  return(ls_array)
}
