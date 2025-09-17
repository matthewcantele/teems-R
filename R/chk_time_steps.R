#' @keywords internal
#' @noRd
.check_time_steps <- function(t0,
                              time_steps,
                              call) {

  if (!as.numeric(time_steps[1]) %=% 0) {
    if (!time_steps[1] %=% t0) {
      .cli_action(data_wrn$time_steps,
                  action = "warn",
                  call = call
      )
    }
    time_steps <- time_steps - time_steps[1]
  }

  if (!all(diff(x = time_steps) > 0)) {
    .cli_action(
      data_err$invalid_time_step,
      action = "abort",
      call = call
    )
  }

  if (.o_verbose()) {
    cli::cli_h1(text = "Time step summary")
    cli::cli_dl(items = c(
      "Actual time steps" = toString(time_steps),
      "Implied chronological years" = toString(t0 + time_steps)
    ))
  }

  return(time_steps)
}