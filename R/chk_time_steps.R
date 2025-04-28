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
        msg = "The first time step provided {.val {implied_t0}}
                       is not consistent with the reference year from the
                       loaded {.arg set_har}: {.val {t0}}.",
        action = "abort",
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
        msg = "The chronological time steps inferred from the
      supplied intervals are {.val {implied_chron}}.",
        action = "inform",
        )
    }
  }

  return(time_steps)
}
