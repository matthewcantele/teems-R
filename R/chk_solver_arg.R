#' @importFrom rlang arg_match
#' 
#' @keywords internal
#' @noRd
.check_solver_arg <- function(n_tasks,
                              n_subintervals,
                              matrix_method,
                              solution_method,
                              steps,
                              paths,
                              call) {
  browser()
  stopifnot(all(is.numeric(x = n_tasks), n_tasks == as.integer(x = n_tasks)))
  stopifnot(all(is.numeric(x = n_subintervals), n_subintervals == as.integer(x = n_subintervals)))
  matrix_method <- rlang::arg_match(arg = matrix_method,
                                    values = c("LU", "DBBD", "SBBD", "NDBBD"))
  solution_method <- rlang::arg_match(arg = solution_method,
                                     values = c("Johansen", "mod_midpoint"))
  if (!(all(steps %% 2 == 0) || all(steps %% 2 == 1))) {
    .cli_action(action = "abort",
                msg = "{.arg n_subintervals} must contain either all even 
                numbers or all odd numbers.",
                call = call)
  }
  if (!all(is.numeric(steps), length(steps) == 3)) {
    .cli_action(action = "abort",
                msg = "{.arg steps} must be a numeric vector of length 3.",
                call = call)
  }
  
  matsol <- switch(
    EXPR = matrix_method,
    "LU" = 0,
    "SBBD" = 1,
    "DBBD" = 2,
    "NDBBD" = 3
  )
  
  tab <- .cmf_retrieve(file = "tabfile",
                       cmf_path = paths[["cmf"]],
                       run_dir = paths[["run"]])
  if (any(grepl(pattern = "(intertemporal)", x = tab))) {
    enable_time <- TRUE
  } else {
    enable_time <- FALSE
  }
  
  if (is.element(el = matsol, set = c("SBBD", "NDBBD")) && !enable_time) {
    stop(paste("Matrix solution method",
               matsol,
               "only applicable to intertemporal model runs."))
  }
  # check int compatibility with LU and DBBD
  
  if (identical(x = matrix_method, y = "NDBBD")) {
    nesteddbbd <- 1
    time_data <- .cmf_retrieve(file = "INTDATA",
                               cmf_path = cmf_path,
                               run_dir = run_dir)
    n_timesteps <- time_data[grep(pattern = "NTSP", x = time_data) + 1]
  } else {
    nesteddbbd <- 0
  }
  if (identical(x = solution_method, y = "mod_midpoint")) {
    step1 <- steps[1]
    step2 <- steps[2]
    step3 <- steps[3]
    solmed <- "Mmid"
  } else {
    solmed <- "Johansen"
    n_subintervals <- 1
  }
}