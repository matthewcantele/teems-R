.check_time_steps <- function(CPHI,
                              LRORG,
                              KAPPA,
                              interval_switch,
                              time_steps,
                              call) {
  if (!interval_switch) {
    t0 <- time_steps[1]
    time_steps <- diff(x = time_steps)
  } else {
    t0 <- NA
  }

  n_timestep <- as.numeric(length(x = time_steps) + 1)
  if (grepl(pattern = "\\.csv", x = CPHI)) {
    CPHI <- .check_input_file(file = CPHI,
                              ext = "csv",
                              call = call)

    .check_time_validity(file = CPHI,
                         col = c("REGr", "ALLTIMEt", "Value"),
                         call = call,
                         n_timestep = n_timestep)
  }

  if (grepl(pattern = "\\.csv", x = KAPPA)) {
    KAPPA <- .check_input_file(file = KAPPA,
                               ext = "csv",
                               call = call)

    .check_time_validity(file = KAPPA,
                         col = c("REGr", "ALLTIMEt", "Value"),
                         call = call,
                         n_timestep = n_timestep)
  }

  if (grepl(pattern = "\\.csv", x = LRORG)) {
    LRORG <- .check_input_file(file = LRORG,
                               ext = "csv",
                               call = call)

    .check_time_validity(file = LRORG,
                         col = c("ALLTIMEt", "Value"),
                         call = call,
                         n_timestep = n_timestep)
  }

ls_int <- list(t0 = t0,
               time_steps = time_steps,
               CPHI = CPHI,
               KAPPA = KAPPA,
               LRORG = LRORG)

return(ls_int)
}
