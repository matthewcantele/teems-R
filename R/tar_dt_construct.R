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
.constructDT <- function(ls_array,
                         metadata,
                         coeff_extract,
                         time_sets,
                         full_exclude,
                         lowercase = TRUE,
                         zCGDS = TRUE,
                         set_array = NULL) {
  data_type <- attr(x = ls_array, "data_type")

  if (!zCGDS) {
    warning('Data.table C-Locale sorting (which is used in associated functions) will lead to errors with capital case "CGDS". This element is found in v6.2 data format.')
  }

  if (!identical(x = metadata[["data_format"]],
                 y = metadata[["model_version"]])) {
    conversion <- TRUE
  } else {
    conversion <- FALSE
  }

  # full exclude on select headers (DREL, etc.)
  ls_array <- subset(
    x = ls_array,
    subset = {
      !is.element(el = names(x = ls_array), set = full_exclude)
    }
  )


  if (identical(x = data_type, y = "par")) {
    # change ETRE set from ENDWS_COMM to ENDW_COMM and add null values for mobile factors
    if (is.element(el = metadata[["database_version"]], set = c("v9", "v10"))) {
      ETRE_data <- purrr::pluck(.x = ls_array, "ETRE", "data")
      ENDWS_dimnames <- unlist(x = dimnames(x = ETRE_data))

      # New elements to be added
      new_elements <- setdiff(
        x = purrr::pluck(.x = set_array, "H6", "data"),
        y = ENDWS_dimnames
      )

      # Combine existing and new elements
      ENDW_elements <- c(ENDWS_dimnames, new_elements)

      # Create a new array with the new dimensions and set all values to 0
      ENDW <- array(0, dim = length(ENDW_elements))
      dimnames(ENDW) <- list(ENDW_COMM = ENDW_elements)

      r_idx <- match(
        x = dimnames(x = ETRE_data)[[1]],
        table = dimnames(x = ENDW)[[1]]
      )

      ENDW[r_idx] <- ETRE_data
      ls_array[["ETRE"]][["data"]] <- ENDW
    }
  }

  # add our package type specific set names
  if (!is.element(el = data_type, set = "set")) {
    r_idx <- match(x = names(x = ls_array), table = coeff_extract[["header"]])
    ls_array <- purrr::map2(
      .x = ls_array,
      .y = r_idx,
      .f = function(header, idx) {
        if (!is.na(idx) && !conversion) {
          # extract won't align so can't pull if conversion == TRUE
          new_names <- setdiff(x = purrr::pluck(coeff_extract, "ls_mixed_idx", idx),
                               y = "ALLTIMEt")
          if (!identical(x = new_names, y = "null_set")) {
            names(x = dimnames(x = header[["data"]])) <- new_names
          }
        } else {
          # this section is for headers that are not read-in but are used in some other way (par weights)
          dimnames_header <- dimnames(x = header[["data"]])
          # for RDLT and other switch/binary
          if (!is.null(x = dimnames_header)) {
            dim_names <- names(x = dimnames_header)
            if (identical(x = sum(grepl(
              pattern = "REG", x = dim_names
            ), na.rm = TRUE), y = 2L)) {
              dim_names[duplicated(x = dim_names)] <- "REGs"
              dim_names <- sub(pattern = "^REG$",
                               replacement = "REGr",
                               x = dim_names)
            } else if (identical(x = sum(grepl(
              pattern = "REG", x = dim_names
            ), na.rm = TRUE), y = 1L)) {
              dim_names <- sub(pattern = "^REG$",
                               replacement = "REGr",
                               x = dim_names)
            }

            # Format- and conversion-specific transformations
            if (!conversion) {
              if (identical(x = metadata[["data_format"]], y = "v6.2")) {
                replace_patterns <- list(
                  "TRAD_COMM" = "TRAD_COMMi",
                  "PROD_COMM" = "PROD_COMMj",
                  "ENDW_COMM" = "ENDW_COMMi"
                )
              } else if (identical(x = metadata[["data_format"]], y = "v7.0")) {
                replace_patterns <- list(
                  "ACTS" = "ACTSa",
                  "COMM" = "COMMc",
                  "DIR" = "DIRd",
                  "ENDW" = "ENDWe"
                )
              }
            } else {
              if (identical(x = metadata[["data_format"]], y = "v6.2")) {
                replace_patterns <- list(
                  "TRAD_COMM" = "COMMc",
                  "PROD_COMM" = "ACTSa",
                  "ENDW_COMM" = "ENDWe"
                )
              } else if (identical(x = metadata[["data_format"]], y = "v7.0")) {
                replace_patterns <- list(
                  "ACTS" = "PROD_COMMj",
                  "COMM" = "TRAD_COMMi",
                  "ENDW" = "ENDW_COMMi",
                  "MARG" = "MARG_COMMm"
                )
              }
            }

            origin <- names(x = replace_patterns)
            r_idx <- match(x = dim_names, table = origin)
            dim_names <- ifelse(
              test = is.element(el = dim_names, set = origin),
              yes = replace_patterns[r_idx],
              no = dim_names
            )

            # updated names
            names(x = dimnames_header) <- dim_names
            dimnames(x = header[["data"]]) <- dimnames_header
          }
        }
        return(header)
      }
    )
  }

  # create list of data.tables from arrays
  # uppercase 'Value' used to distinguish from automatically generated columns
  # single dimension header (e.g., SAVE, POP, etc.)
  ls_array <- lapply(X = ls_array, FUN = function(header) {
    # set file
    if (identical(
      x = length(x = dimnames(x = header[["data"]])),
      y = 0L
    )) {
      header[["dt"]] <- data.table::as.data.table(x = header[["data"]])
      if (!identical(x = data_type, y = "set")) {
        data.table::setnames(x = header[["dt"]], new = "Value")
      } else if (identical(x = data_type, y = "set")) {
        data.table::setnames(x = header[["dt"]], new = header[["header_name"]])
      }
    } else if (identical(
      x = length(x = dimnames(x = header[["data"]])),
      y = 1L
    )) {
      header[["dt"]] <- data.table::as.data.table(
        x = cbind(Value = as.list(x = header[["data"]])),
        keep.rownames = names(x = dimnames(x = header[["data"]]))
      )
    } else if (identical(
      x = length(x = dimnames(x = header[["data"]])),
      y = 2L
    )) {
      # 2 dimension header (e.g., VST etc.)
      header[["dt"]] <- data.table::as.data.table(x = as.table(x = header[["data"]]))
      # the key argument could be used but no need to set keys here
      data.table::setnames(header[["dt"]], old = "N", new = "Value")
    } else {
      # more than 2 dimensions
      header[["dt"]] <- data.table::as.data.table(x = header[["data"]], value.name = "Value")
    }

    # ensure that all values coercible to numeric are classed as numeric
    if (!identical(x = data_type, y = "set")) {
      if (all(!is.na(x = as.numeric(x = purrr::pluck(.x = header, "dt", "Value"))))) {
        purrr::pluck(.x = header, "dt", "Value") <- as.numeric(x = purrr::pluck(.x = header, "dt", "Value"))
      }
    }
    return(header)
  })

  # if tab file format data format mismatch convert data
  if (conversion) {
    ls_array <- .convert_data_format(data = ls_array,
                                     data_format = metadata[["data_format"]],
                                     data_type = data_type)
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
