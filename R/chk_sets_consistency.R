#' @keywords internal
#' @noRd
.check_set_consistency <- function(bin_sets,
                                   tab_sets,
                                   call) {
  if (!all(names(tab_sets) %in% bin_sets$setname)) {
    x_sets <- setdiff(names(tab_sets), bin_sets$setname)
    .cli_action(compose_err$missing_sets,
                action = "abort",
                call = call)
  }
  bin_sets <- subset(bin_sets,
                     setname %in% names(tab_sets))
  r_idx <- match(bin_sets$setname, names(tab_sets))
  if (!all(purrr::map2_lgl(tab_sets[r_idx],
              bin_sets$ele,
              all.equal))) {
    .cli_action(compose_err$set_mismatch,
      action = "abort",
      call = call
    )
  }

  return(invisible(NULL))
}