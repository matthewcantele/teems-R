#' @importFrom purrr map_lgl
#'
#' @keywords internal
#' @noRd
.match_year <- function(data,
                        sets,
                        time_steps) {
  int_sets <- sets[purrr::map_lgl(sets, function(s) {
    isTRUE(attr(s, "intertemporal"))
  })]
  stnd_names <- .dock_tail(colnames(data))

  if (any(stnd_names %in% names(int_sets))) {
    TIMEt <- colnames(data)[which(stnd_names %in% names(int_sets))]
    r_idx <- match(data[[TIMEt]], time_steps$all_time)
    data[, Year := time_steps$CYRS[r_idx]]
    return(data)
  } else {
    return(data)
  }
}