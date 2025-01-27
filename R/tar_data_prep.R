#' Aggregate Data
#'
#'
#' This function aggregates data based on the provided parameters. It performs
#' operations such as set mapping, summing value columns by sets, creating
#' specific sets for lead purposes, and expanding data under certain conditions.
#'
#' @inheritParams .write_sets
#' @param data_type Either "dat", "par", or "set.
#' @param tib_data A tibble. The data to be aggregated produced by
#'   \code{.build_tibble()}.
#' @param data_format Character string. The format of GTAP data, either "v6.2"
#'   or "v7.0".
#' @param coeff_list A list of model coefficients produced by \code{.parse_tablo()}.
#'
#' @importFrom purrr pluck
#' @importFrom data.table setnames let rbindlist CJ setcolorder
#' @return A data frame containing the aggregated data.
#' @keywords internal
#' @noRd
.prep_data <- function(data_type,
                       tib_data,
                       sets,
                       coeff_list,
                       data_format) {
  # all set mappings consolidated
  full_sets <- subset(x = sets, subset = {!is.na(header)}, select = full_sets)[[1]]
  lapply(X = full_sets,
         FUN = data.table::setnames,
         new = c("origin", "map"))

  if (!identical(x = data_type, y = "set")) {
    if (identical(x = data_type, y = "par")) {
      # check that all headers to read are present
      # these core files should be passed in
      core_file <- "GTAPPARM"
    } else if (identical(x = data_type, y = "dat")) {
      core_file <- "GTAPDATA"
    }

    req_headers <- unlist(x = subset(x = coeff_list,
                                     subset = {
                                       is.element(el = file, set = core_file)
                                     },
                                     select = header))

    if (!all(is.element(el = req_headers, set = tib_data[["header"]]))) {
      missing_headers <- toString(x = req_headers[!is.element(el = req_headers, set = tib_data[["header"]])])
      stop(paste("Headers determined as required:",
                 missing_headers,
                 "were not loaded."))
    }
    # remove unnecessary (not written) headers
    tib_data <- subset(x = tib_data,
                       subset = {
                         is.element(el = header, set = req_headers)
                       })
  }

  # PROD_COMM isn't read in so it isn't needed elsewhere
  if (identical(x = data_format, y = "v6.2")) {
    full_sets[["PROD_COMM"]] <- data.table::rbindlist(l = list(full_sets[["TRAD_COMM"]],
                                                               full_sets[["CGDS_COMM"]]))
  }

  # new set mappings (note use of is.character to differential set col from value col)
  tib_data[["dt"]] <- lapply(
    X = tib_data[["dt"]],
    FUN = function(dt) {
      set_col_mixed <- colnames(x = dt)[sapply(X = dt, FUN = is.character)]
      for (c in seq_along(set_col_mixed)) {
        set_col <- set_col_mixed[c]
        set_map <- substr(x = set_col,
                          start = 1,
                          stop = nchar(x = set_col) - 1)
        table <- full_sets[[set_map]]
        r_idx <- match(x = dt[[set_col]], table = table[["origin"]])
        dt[, (set_col) := lapply(.SD, function(r) {
          table[["map"]][r_idx]
        }), .SDcols = set_col]
      }
      return(dt)
    }
  )

  return(tib_data)
}
