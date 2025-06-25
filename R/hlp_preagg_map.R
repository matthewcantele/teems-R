#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.preagg_map <- function(dt,
                        sets) {
  ls_mixed_idx <- colnames(x = dt)[sapply(X = dt, FUN = is.character)]
  for (c in seq_along(ls_mixed_idx)) {
    set_col <- ls_mixed_idx[c]
    set_map <- .dock_tail(string = set_col)
    table <- purrr::pluck(.x = sets, "mapping", set_map)
    r_idx <- match(x = dt[[set_col]], table = table[["origin"]])
    dt[, (set_col) := lapply(.SD, function(r) {
      table[["mapping"]][r_idx]
    }), .SDcols = set_col]
  }
  return(dt)
}
