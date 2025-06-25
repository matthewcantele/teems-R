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
  if (!isTRUE(x = is.na(x = int_sets))) {
    int_set_names <- int_sets[["header"]]
    intertemporal <- TRUE
  } else {
    intertemporal <- FALSE
  }

  ls_array <- lapply(X = ls_array, FUN = function(header) {
    dim_length <- length(x = dimnames(x = header[["data"]]))
    # set file
    if (identical(x = dim_length, y = 0L)) {
      header[["dt"]] <- data.table::as.data.table(x = as.matrix(x = header[["data"]]))
      if (!is_set_array) {
        data.table::setnames(x = header[["dt"]], new = "Value")
      } else {
        data.table::setnames(x = header[["dt"]], new = header[["header"]])
      }
    } else {
      header[["dt"]] <- array2DF(x = header[["data"]])
      stnd_col <- .dock_tail(string = colnames(x = header[["dt"]]))
      if (intertemporal) {
      if (any(is.element(el = stnd_col, set = int_set_names))) {
        int_col <- colnames(header[["dt"]])[is.element(el = stnd_col, set = int_set_names)]
        header[["dt"]][[int_col]] <- as.integer(x = header[["dt"]][[int_col]])
      }
      }
      data.table::setDT(x = header[["dt"]])
    } 
    return(header)
  })

  # header_replace if != NULL
  if (!is.null(x = unaggregated_input)) {
    ls_array <- .inject_unagg_input(ls_data = ls_array,
                                    unaggregated_input = unaggregated_input)

  }
  
  # create and write out metadata file
  # if arrays are desired this is where they enter
  metadata <- data.table::rbindlist(l = lapply(X = ls_array, FUN = function(header) {
    list(
      header = header[["header"]],
      label = header[["label"]],
      coefficient = header[["coefficient"]]
    )
  }))

  # convert to tibble
  tib_data <- tibble::as_tibble(x = metadata)

  # add write to file here
  if (!is_set_array) {
    r_idx <- match(x = tib_data[["header"]], table = coeff_extract[["header"]])
    tib_data[["file"]] <- coeff_extract[["file"]][r_idx]
    tib_data[["data_type"]] <- ifelse(test = grepl(pattern = "parameter",
                                                   x = coeff_extract[["qualifier_list"]][r_idx]),
                                      yes = "par",
                                      no = ifelse(test = is.element(el = tib_data[["header"]],
                                                                    set = coeff_extract[["header"]]),
                                                  yes = "dat",
                                                  no = NA))
    tib_data[["type"]] <- ifelse(test = grepl(pattern = "integer",
                                              x = coeff_extract[["qualifier_list"]][r_idx]),
                                      yes = "integer",
                                      no = ifelse(test = is.element(el = tib_data[["header"]],
                                                                    set = coeff_extract[["header"]]),
                                                  yes = "real",
                                                  no = NA))
  } else {
    r_idx <- match(x = tib_data[["header"]], table = set_extract[["header"]])
    tib_data[["file"]] <- set_extract[["file"]][r_idx]
    tib_data[["data_type"]] <- "set"
    tib_data[["type"]] <- "character"
  }

  # join metadata with data (match unnecessary but to be safe...)
  r_idx <- match(x = names(x = ls_array), table = tib_data[["header"]])
  
  # check metadata/data match
  if (any(is.na(x = r_idx))) {
    stop("NA found in match between metadata and data")
  }

  tib_data[["dt"]] <- lapply(X = ls_array, FUN = function(d) {
    d[["dt"]]
  })[r_idx]

  # check for mismatches
  if (!all(is.element(el = names(x = tib_data[["dt"]]),set = tib_data[["header"]]))) {
    stop("Name mismatch after metadata/data merge")
  }

  # names for later
  names(x = tib_data[["label"]]) <- tib_data[["header"]]
  names(x = tib_data[["type"]]) <- tib_data[["header"]]

  # sort data
  lapply(X = tib_data[["dt"]], FUN = data.table::setkey)

  return(tib_data)
}