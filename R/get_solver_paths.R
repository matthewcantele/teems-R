#' @keywords internal
#' @noRd
.get_solver_paths <- function(cmf_path,
                              call) {
  if (!file.exists(cmf_path)) {
    .cli_action(action = "abort",
                msg = "The {.arg cmf_path} provided {.path {cmf_path}} does not
                exist.",
                call = call)
  }
  run_dir <- dirname(path = cmf_path)
  timeID <- format(x = Sys.time(), "%H%M")
  diagnostic_out <- file.path(run_dir,
                              "out",
                              paste0("solver_out", "_", timeID, ".txt"))
  docker_run_dir <- "/home/launchpad"
  docker_cmf_path <- sub(pattern = dirname(path = cmf_path),
                         replacement = docker_run_dir,
                         x = cmf_path)
  paths <- list(cmf = unname(obj = cmf_path),
                run = run_dir,
                diag_out = diagnostic_out,
                docker_run = docker_run_dir,
                docker_cmf = unname(obj = docker_cmf_path))
  return(paths)
}