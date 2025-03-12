#' @keywords internal
#' @noRd
.solve_model <- function(exec_cmd,
                         sol_parse_cmd,
                         paths,
                         quiet) {

  elapsed_time <- system.time(system(command = exec_cmd))
  print(elapsed_time)
  cat(exec_cmd, file = file.path(paths[["run"]], "model_exec.txt"))
  out_file <- readLines(con = paths[["diag_out"]])
  if (any(grepl(pattern = "Error", x = out_file))) {
    error <- TRUE
  } else {
    error <- FALSE
  }
  if (any(grepl(pattern = "singular", x = out_file))) {
    singularity <- TRUE
  } else {
    singularity <- FALSE
  }
  if (!error && !singularity) {
    system(command = sol_parse_cmd, ignore.stdout = TRUE)
  }
  .check_diagnostics(
    out_file = out_file,
    error = error,
    singularity = singularity,
    quiet = quiet
  )
  return(invisible(NULL))
}