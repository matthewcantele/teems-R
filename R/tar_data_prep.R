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
#'   or "v7".
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
                       coeff_list) {

  # all set mappings consolidated
  full_sets <- subset(x = sets, subset = {!is.na(header)}, select = full_sets)
  stacked_full_sets <- unique(x = data.table::rbindlist(l = purrr::pluck(full_sets, 1), use.names = FALSE))
  stacked_full_sets <- data.table::setnames(x = stacked_full_sets, new = c("origin", "map"))

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
      missing_headers <- req_headers[!is.element(el = req_headers, set = tib_data[["header"]])]
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

  # new set mappings (note use of is.character to differential set col from value col)
  tib_data[["dt"]] <- lapply(X = tib_data[["dt"]], FUN = function(d) {
    d <- d[, lapply(X = .SD, FUN = function(char_col) {
      if (is.character(x = char_col)) {
        r_idx <- match(x = char_col, table = stacked_full_sets[["origin"]])
        char_col <- stacked_full_sets[["map"]][r_idx]
      }
      return(char_col)
    })]
  })

  return(tib_data)
}
