#' @keywords internal
#' @noRd
.check_time_steps <- function(t0,
                              time_steps,
                              interval_switch,
                              args_list,
                              call,
                              quiet) {
  if (!interval_switch) {
    implied_t0 <- time_steps[1]
    if (!identical(x = t0, y = implied_t0)) {
      .cli_action(
        action = "abort",
        msg = "The first time step provided {.val {implied_t0}}
                       is not consistent with the reference year from the
                       loaded {.arg set_har}: {.val {t0}}.",
        # url = "https://r-project.org",
        # hyperlink = "test",
        call = call
      )
    }
    time_steps <- diff(x = time_steps)
  } else {
    implied_chron <- c(t0, t0 + cumsum(time_steps))
    if (!quiet) {
      .cli_action(
        action = "inform",
        msg = "The chronological time steps inferred from the
      supplied intervals are {.val {implied_chron}}."
        )
    }
  }

  return(time_steps)
}
