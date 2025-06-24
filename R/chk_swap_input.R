#' @keywords internal
.check_swap_input <- function(raw_swap,
                              var_extract,
                              direction,
                              call) {

  # if (!is.null(x = raw_swap)) {
  #   if (!is.swap(x = raw_swap)) {
  #     # single character
  #     if (is.list(x = raw_swap)) {
  #       # swap in a list
  #       if (is.swap(x = raw_swap[[1]])) {
  #         processed_swap <- raw_swap[[1]]
  #       } else {
  #         .cli_action(
  #           msg = "Invalid list object supplied as swap.",
  #           action = "abort",
  #           call = call
  #         )
  #       }
  #     } else {
  #       processed_swap <- do.call(what = teems_swap, args = list(raw_swap))[[1]]
  #     }
  #   } else {
  #     # single swap
  #     processed_swap <- raw_swap
  #   }
  # }
  
  if (!is.null(x = raw_swap)) {
    # Handle each case directly
    if (is.swap(x = raw_swap)) {
      processed_swap <- raw_swap
    } else if (is.list(x = raw_swap) && is.swap(x = raw_swap[[1]])) {
      processed_swap <- raw_swap[[1]]
    } else if (is.list(x = raw_swap)) {
      .cli_action(
        msg = "Invalid list object supplied as swap.",
        action = "abort",
        call = call
      )
    } else {
      processed_swap <- do.call(what = teems_swap, args = list(raw_swap))[[1]]
    }
  }
  

  if (!is.element(el = processed_swap[["var"]], set = var_extract[["name"]])) {
    var_name <- processed_swap[["var"]]
    .cli_action(msg = "The variable {.val {var_name}} designed for a swap 
    {direction} is not found within the model Tablo file provided.",
                action = "abort",
                call = call)
  }
  
  if (!isTRUE(is.na(x = processed_swap[["swap_sets"]]))) {
    ls_mixed <- purrr::pluck(.x = var_extract, "ls_mixed_idx", processed_swap[["var"]])

    # check that the names provided are correct
    if (!all(is.element(
      el = processed_swap[["swap_sets"]],
      set = ls_mixed
    ))) {
      non_exist_set <- setdiff(x = processed_swap[["swap_sets"]], y = ls_mixed)
      var_name <- processed_swap[["var"]]
      .cli_action(msg = c("The swap-{direction} set {.val {non_exist_set}} is not 
                  associated with the variable {.val {var_name}}.",
                          "Note that set designations within {.pkg teems} are 
                          comprised of the variable-specific uppercase set name and 
                          lowercase index.",
                          "For {.val {var_name}} these include: {.field {ls_mixed}}."),
                  action = c("abort", "inform", "inform"),
                  call = call)
    }

    if (!identical(x = processed_swap[["swap_sets"]], y = ls_mixed)) {
      missing_sets <- setdiff(x = ls_mixed, y = processed_swap[["swap_sets"]])
      ls_missing_sets <- as.list(x = missing_sets)
      names(x = ls_missing_sets) <- missing_sets
      processed_swap[["swap_sets"]] <-  c(processed_swap[["swap_ele"]], ls_missing_sets)
      # ordering
      r_idx <- match(x = ls_mixed, table = names(x = processed_swap[["swap_sets"]]))
      processed_swap[["swap_sets"]] <- processed_swap[["swap_sets"]][r_idx]
    }
  }
  
  processed_swap <- processed_swap[!is.element(el = names(x = processed_swap),
                                               set = "swap_ele")]
  
  return(processed_swap)
}