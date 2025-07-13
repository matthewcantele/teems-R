#' @importFrom tibble as_tibble
#' @importFrom data.table as.data.table setnames setDT rbindlist setkey
#' 
#' @keywords internal
#' @noRd
.build_tibble <- function(ls_array,
                          is_set_array = FALSE,
                          set_extract,
                          int_sets,
                          unaggregated_input = NULL,
                          coeff_extract = NULL) {
  if (!isTRUE(is.na(int_sets))) {
    int_set_names <- int_sets$header
    intertemporal <- TRUE
  } else {
    intertemporal <- FALSE
  }

  ls_array <- lapply(ls_array, function(header) {
    dim_length <- length(dimnames(header$data))
    # set file
    if (dim_length %=% 0L) {
      header$dt <- data.table::as.data.table(as.matrix(header$data))
      if (!is_set_array) {
        data.table::setnames(header$dt, new = "Value")
      } else {
        data.table::setnames(header$dt, new = header$header)
      }
    } else {
      header$dt <- array2DF(header$data)
      stnd_col <- .dock_tail(string = colnames(header$dt))
      if (intertemporal) {
        if (any(stnd_col %in% int_set_names)) {
          int_col <- colnames(header$dt)[stnd_col %in% int_set_names]
          header[["dt"]][[int_col]] <- as.integer(header[["dt"]][[int_col]])
        }
      }
      data.table::setDT(header$dt)
    }
    return(header)
  })


  if (!is.null(unaggregated_input)) {
    ls_array <- .inject_unagg_input(
      ls_data = ls_array,
      unaggregated_input = unaggregated_input
    )
  }

  # create and write out metadata file
  # if arrays are desired this is where they enter
  metadata <- data.table::rbindlist(lapply(ls_array, function(header) {
    list(
      header = header$header,
      label = header$label,
      coefficient = header$coefficient
    )
  }))

  # convert to tibble
  tib_data <- tibble::as_tibble(metadata)

  # add write to file here
  if (!is_set_array) {
    r_idx <- match(tib_data$header, coeff_extract$header)
    tib_data$file <- coeff_extract$file[r_idx]
    tib_data$data_type <- ifelse(grepl(
      "parameter",
      coeff_extract$qualifier_list[r_idx]
    ),
    "par",
    ifelse(tib_data$header %in% coeff_extract$header,
      "dat",
      NA
    )
    )
    tib_data$type <- ifelse(grepl(
      "integer",
      coeff_extract$qualifier_list[r_idx]
    ),
    "integer",
    ifelse(tib_data$header %in% coeff_extract$header,
      "real",
      NA
    )
    )
  } else {
    r_idx <- match(tib_data$header, set_extract$header)
    tib_data$file <- set_extract$file[r_idx]
    tib_data$data_type <- "set"
    tib_data$type <- "character"
  }
  # join metadata with data (match unnecessary but to be safe...)
  r_idx <- match(names(ls_array), tib_data$header)

  # check metadata/data match
  if (any(is.na(r_idx))) {
    stop("NA found in match between metadata and data")
  }

  tib_data$dt <- lapply(ls_array, function(d) {
    d$dt
  })[r_idx]

  # check for mismatches
  if (!all(names(tib_data$dt) %in% tib_data$header)) {
    stop("Name mismatch after metadata/data merge")
  }

  # names for later
  names(tib_data$label) <- tib_data$header
  names(tib_data$type) <- tib_data$header

  # sort data
  lapply(tib_data$dt, data.table::setkey)

  return(tib_data)
}