#' @importFrom data.table setnames rbindlist
#' 
#' @keywords internal
#' @noRd
.prep_data <- function(tib_data,
                       sets,
                       coeff_extract) {

  req_headers <- unlist(x = subset(
    x = coeff_extract,
    subset = {!is.na(x = file)},
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
  
  # merge required coeff_extract information
  r_idx <- match(x = tib_data[["coefficient"]],
                 table = coeff_extract[["coefficient"]])
  
  tib_data[["ls_upper_idx"]] <- coeff_extract[["ls_upper_idx"]][r_idx]
  tib_data[["ls_mixed_idx"]] <- coeff_extract[["ls_mixed_idx"]][r_idx]
  
  # mappings
  tib_data[["dt"]] <- lapply(X = tib_data[["dt"]],
                             FUN = function(dt) {

                               ls_mixed_idx <- colnames(x = dt)[sapply(X = dt, FUN = is.character)]
                               for (c in seq_along(ls_mixed_idx)) {
                                 set_col <- ls_mixed_idx[c]
                                 set_map <- substr(
                                   x = set_col,
                                   start = 1,
                                   stop = nchar(x = set_col) - 1
                                 )
                                 table <- purrr::pluck(.x = sets, "mapping", set_map)
                                 r_idx <- match(x = dt[[set_col]], table = table[["origin"]])
                                 dt[, (set_col) := lapply(.SD, function(r) {
                                   table[["mapping"]][r_idx]
                                 }), .SDcols = set_col]
                               }
                               return(dt)
                             })

  return(tib_data)
}
