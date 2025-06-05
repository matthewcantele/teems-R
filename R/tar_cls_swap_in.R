#' @keywords internal
#' @noRd
.swap_in <- function(closure,
                     swap_in,
                     sets,
                     var_extract) {
  if (!is.null(x = swap_in)) {
    concat_swap_in <- lapply(
      X = swap_in,
      FUN = .check_swap,
      var_extract = var_extract,
      sets = sets
    )

    new_entries <- unlist(x = concat_swap_in)

    # run check and expansion on new entries
    checked_entries <- .check_closure(
      closure = new_entries,
      sets = sets
    )

    expanded_entries <- .expand_closure(
      closure = checked_entries,
      var_extract = var_extract,
      sets = sets
    )

    # bind for current closure
    updated_closure <- rbind(expanded_entries, closure)
  } else {
    updated_closure <- closure
  }
  return(updated_closure)
}
