#' @importFrom rlang arg_match is_integerish
#' 
#' @keywords internal
#' @noRd
.validate_solver_args <- function(n_tasks,
                                  n_subintervals,
                                  matrix_method,
                                  solution_method,
                                  steps,
                                  n_timesteps,
                                  paths,
                                  call) {
  if (!rlang::is_integerish(n_tasks)) {
    arg <- "n_tasks"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (!as.integer(length(n_tasks)) %=% 1L) {
    arg <- "n_tasks"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  if (!rlang::is_integerish(n_subintervals)) {
    arg <- "n_subintervals"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (!as.integer(length(n_subintervals)) %=% 1L) {
    arg <- "n_subintervals"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  matrix_method <- rlang::arg_match(
    arg = matrix_method,
    values = c("LU", "DBBD", "SBBD", "NDBBD")
  )
  solution_method <- rlang::arg_match(
    arg = solution_method,
    values = c("Johansen", "mod_midpoint")
  )
  if (!(all(steps %% 2 == 0) || all(steps %% 2 == 1))) {
    .cli_action(solve_err$subint_form,
      action = "abort",
      call = call
    )
  }
  if (!all(is.numeric(steps), length(steps) == 3)) {
    .cli_action(solve_err$step_length,
      action = "abort",
      call = call
    )
  }

  matsol <- switch(
    EXPR = matrix_method,
    "LU" = 0,
    "SBBD" = 1,
    "DBBD" = 2,
    "NDBBD" = 3
  )

  if ("tab_path" %in% names(attributes(paths$cmf))) {
    tab <- readLines(attr(paths$cmf, "tab_path"))
  } else {
    tab <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = paths$cmf
    )
    tab <- readLines(tab)
  }

  if (any(grepl(pattern = "(intertemporal)", tab))) {
    enable_time <- TRUE
  } else {
    enable_time <- FALSE
  }

  if (matrix_method %in% c("SBBD", "NDBBD") && !enable_time) {
    .cli_action(solve_err$invalid_method,
      action = "abort",
      call = call
    )
  }

  if (identical(x = matrix_method, y = "NDBBD")) {
    # this warning necessary in NULL method
    if (is.null(n_timesteps)) {
      .cli_action(solve_err$NDBBD_time,
                  action = "abort",
                  call = call)
    }
    nesteddbbd <- 1
  } else {
    nesteddbbd <- 0
  }
  if (solution_method %=% "mod_midpoint") {
    solmed <- "Mmid"
  } else {
    solmed <- "Johansen"
    n_subintervals <- 1
  }

  mod_arg <- list(
    matsol = matsol,
    solmed = solmed,
    n_subintervals = n_subintervals,
    n_timesteps = n_timesteps,
    nesteddbbd = nesteddbbd,
    enable_time = enable_time
  )

  return(mod_arg)
}