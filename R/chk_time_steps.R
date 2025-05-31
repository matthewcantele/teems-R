#' @keywords internal
#' @noRd
.check_time_steps <- function(t0,
                              time_steps,
                              timestep_coeff,
                              n_timestep_coeff,
                              time_format,
                              quiet,
                              call) {

  if (identical(x = time_format, y = "chronological")) {
    implied_t0 <- time_steps[1]
    if (!identical(x = t0, y = implied_t0)) {
      .cli_action(
        msg = "The first time step provided {.val {implied_t0}}
                       is not consistent with the reference year from the
                       loaded {.arg set_har}: {.val {t0}}.",
        action = "warn",
        # url = "https://r-project.org",
        # hyperlink = "test",
        call = call
      )
    }
    if (!all(diff(x = time_steps) > 0)) {
      .cli_action(
        msg = "One or more {.arg chronologicaltime steps does not progress into the future.",
        action = "abort",
        call = call
      )
    }
    f_timesteps <- c(0, cumsum(x = diff(x = time_steps)))
  } else if (identical(x = time_format, y = "interval")) {
    if (!all(time_steps[-1] > 0)) {
      .cli_action(
        msg = c(
          "One or more {.arg interval} time steps beyond t0 does not progress 
          into the future.", "Note that when {.arg time_format} is set to
                {.val {time_format}}, the initial interval represents the gap
                between t0 and the successive step, i.e all intervals must be
                positive integers."
        ),
        action = c("abort", "inform"),
        call = call
      )
    }
    f_timesteps <- cumsum(x = time_steps)
  } else if (identical(x = time_format, y = "diff")) {
    if (!all(time_steps >= 0)) {
      .cli_action(
        msg = "One or more {.arg diff} time steps does not progress into the future.",
        action = "abort",
        call = call
      )
    }
    f_timesteps <- time_steps
  }

  if (!quiet) {
    implied_chron <- switch(EXPR = time_format,
                            "chronological" = time_steps,
                            "interval" = t0 + cumsum(time_steps),
                            "diff" = time_steps + t0)
    
    implied_intervals <- switch(EXPR = time_format,
                                "chronological" = c(0, diff(x = time_steps)),
                                "interval" = time_steps,
                                "diff" = c(0, diff(x = time_steps)))
    
    implied_diff <- switch(EXPR = time_format,
                           "chronological" = cumsum(x = diff(x = time_steps)),
                           "interval" = cumsum(time_steps),
                           "diff" = time_steps)
    
    cli::cli_h1(text = "Time step summary")
    cli::cli_dl(items = c("Chronological years" = toString(x = implied_chron),
                          "Intervals from t0" = toString(x = implied_intervals),
                          "Difference from t0" = toString(x = implied_diff)))
  }
  
  f_timesteps <- list(f_timesteps, length(x = f_timesteps))
  names(x = f_timesteps) <- c(timestep_coeff, n_timestep_coeff)
  return(f_timesteps)
}
