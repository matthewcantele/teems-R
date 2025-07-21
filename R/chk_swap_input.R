#' @importFrom purrr pluck
#' 
#' @keywords internal
.check_swap_input <- function(raw_swap,
                              var_extract,
                              direction,
                              call) {

  call <- attr(raw_swap, "call")
  call_id <- attr(raw_swap, "call_id")
  #if (!is.null(raw_swap)) {
    # Handle each case directly
    if (.is_swap(raw_swap)) {
      processed_swap <- raw_swap
    } else if (is.list(raw_swap) && .is_swap(raw_swap[[1]])) {
      processed_swap <- raw_swap[[1]]
    } else if (is.list(raw_swap)) {
      .cli_action(
        swap_err$invalid_swap,
        action = "abort",
        call = call
      )
    } else {
      processed_swap <- do.call(ems_swap, list(raw_swap))[[1]]
    }
  #}


  if (!processed_swap$var %in% var_extract$name) {
    var_name <- processed_swap$var
    .cli_action(swap_err$no_var,
      action = "abort",
      call = call
    )
  }

  if (!isTRUE(is.na(processed_swap$swap_sets))) {
    ls_mixed <- purrr::pluck(var_extract, "ls_mixed_idx", processed_swap$var)

    # check that the names provided are correct
    if (!all(processed_swap$swap_sets %in% ls_mixed)) {
      non_exist_set <- setdiff(processed_swap$swap_sets, ls_mixed)
      var_name <- processed_swap$var
      .cli_action(
        swap_err$invalid_set,
        action = c("abort", "inform", "inform"),
        url = NULL,
        hyperlink = NULL,
        call = call
      )
    }

    if (!identical(processed_swap$swap_sets, ls_mixed)) {
      missing_sets <- setdiff(ls_mixed, y = processed_swap$swap_sets)
      ls_missing_sets <- as.list(missing_sets)
      names(ls_missing_sets) <- missing_sets
      processed_swap$swap_sets <- c(processed_swap$swap_ele, ls_missing_sets)
      # ordering
      r_idx <- match(ls_mixed, names(processed_swap$swap_sets))
      processed_swap$swap_sets <- processed_swap$swap_sets[r_idx]
    }
  }

  processed_swap <- processed_swap[!names(processed_swap) %in% "swap_ele"]

  attr(processed_swap, "call") <- call
  attr(processed_swap, "call_id") <- call_id
  return(processed_swap)
}