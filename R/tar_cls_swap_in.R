#' Swap In
#'
#' This function checks variables that have been swapped in and add them to the
#' closure.
#'
#' @param closure Character vector. The closure file to be used. Must be
#'   provided if standardClosure is FALSE.
#' @param swap_in Character vector. Variables to be added to the closure.
#'   Default is NULL.
#' @param sets A list containing set definitions and their elements.
#'
#' @importFrom purrr pluck pluck_depth
#' @return A list containing the closure and the closure file.
#' @keywords internal
#' @noRd
.swap_in <- function(closure,
                     swap_in,
                     sets,
                     var_extract) {
browser()
  if (!is.null(x = swap_in)) {
    # convoluted but appears to work
    list_depth <- purrr::pluck_depth(x = swap_in)
    if (!is.null(x = purrr::pluck(.x = swap_in, list_depth))) {
      swap_in <- list(swap_in)
    }

    swap_in <- lapply(X = swap_in, FUN = .swap_reconstruct)

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
