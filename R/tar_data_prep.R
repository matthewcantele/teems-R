#' @importFrom data.table setnames rbindlist
#' 
#' @keywords internal
#' @noRd
.prep_data <- function(data_type,
                       tib_data,
                       sets,
                       coeff_list,
                       data_format) {

  # all set mappings consolidated
  full_sets <- subset(x = sets, subset = {
    !is.na(header)
  }, select = full_sets)[[1]]
  lapply(
    X = full_sets,
    FUN = data.table::setnames,
    new = c("origin", "map")
  )

  type <- data_type
  req_headers <- unlist(x = subset(
    x = coeff_list,
    subset = {
      is.element(el = data_type, set = type) &
        !is.na(x = file)
    },
    select = header
  ))

  if (!all(is.element(el = req_headers, set = tib_data[["header"]]))) {
    missing_headers <- toString(x = req_headers[!is.element(el = req_headers, set = tib_data[["header"]])])
    stop(paste(
      "Headers determined as required:",
      missing_headers,
      "were not loaded."
    ))
  }
  # remove unnecessary (not written) headers
  tib_data <- subset(
    x = tib_data,
    subset = {
      is.element(el = header, set = req_headers)
    }
  )

  # PROD_COMM isn't read in so it isn't needed elsewhere
  if (identical(x = data_format, y = "v6.2")) {
    full_sets[["PROD_COMM"]] <- data.table::rbindlist(l = list(
      full_sets[["TRAD_COMM"]],
      full_sets[["CGDS_COMM"]]
    ))
  }

  # new set mappings (note use of is.character to differential set col from value col)
  tib_data[["dt"]] <- purrr::map2(.x = tib_data[["dt"]],
                                  .y = tib_data[["aggregate"]],
                                  .f = function(dt, agg) {
                                    if (agg) {
                                    set_col_mixed <- colnames(x = dt)[sapply(X = dt, FUN = is.character)]
                                    for (c in seq_along(set_col_mixed)) {
                                      set_col <- set_col_mixed[c]
                                      set_map <- substr(
                                        x = set_col,
                                        start = 1,
                                        stop = nchar(x = set_col) - 1
                                      )
                                      table <- full_sets[[set_map]]
                                      r_idx <- match(x = dt[[set_col]], table = table[["origin"]])
                                      dt[, (set_col) := lapply(.SD, function(r) {
                                        table[["map"]][r_idx]
                                      }), .SDcols = set_col]
                                    }
                                    }
                                    return(dt)
                                  })

  return(tib_data)
}
