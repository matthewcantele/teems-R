#' @keywords internal
#' @noRd
.construct_cmd <- function(paths,
                           terminal_run,
                           docker_tag,
                           timeID,
                           n_tasks,
                           n_subintervals,
                           solmed,
                           nesteddbbd,
                           n_timesteps,
                           laA,
                           laDi,
                           laD,
                           matsol,
                           steps,
                           enable_time) {
 
  docker_preamble <- paste(
    "docker run --rm --privileged --volume",
    paste0(paths[["run"]], ":/home/launchpad"),
    paste0("teems", ":", docker_tag),
    "/bin/bash -c"
  )
  exec_preamble <- paste(
    docker_preamble,
    '"singularity exec --bind /home/launchpad /home/solver.sif /home/teems-solver/lib/mpi/bin/mpiexec',
    "-n", n_tasks,
    "/home/teems-solver/solver/hsl",
    "-cmdfile", paths[["docker_cmf"]]
  )
  docker_diagnostic_out <- file.path(paths[["docker_run"]], "out", paste0("solver_out", "_", timeID, ".txt"))
  solver_param <- paste(
    "-matsol", matsol,
    if (identical(x = solmed, y = "Mmid")) {
      paste("-step1", steps[1], "-step2", steps[2], "-step3", steps[3])
    },
    if (any(is.element(el = matsol, set = c(0, 2, 3)))) {
      paste("-regset", "REG")
    },
    if (enable_time) {
      "-enable_time"
    },
    "-nsubints", n_subintervals,
    "-solmed", solmed,
    "-nesteddbbd", nesteddbbd,
    if (identical(x = matsol, y = 3)) {
      paste('-ndbbd_bl_rank', n_timesteps)
    },
    "-presol", 1,
    "-laA", laA,
    "-laDi", laDi,
    "-laD", laD,
    paste0("-x OMP_NUM_THREADS=", 1),
    paste("-maxthreads", 1),
    "| tee",
    paste(docker_diagnostic_out, '&& chown -R 1000:1000', paste0(paths[["docker_run"]], '"'))
  )

  exec_cmd <- paste(exec_preamble, solver_param)
  sol_parse_cmd <- paste(sub(pattern = "--privileged ", replacement = "", x = docker_preamble),
                         '"make -C /home/sol_parser && chown -R 1000:1000 /home/launchpad/out/variables/bin"')
  if (terminal_run) {
    cat(cmd, file = file.path(paths[["run"]], "model_exec.txt"))
    hsl <- "hsl"
    diag_out <- paths[["diag_out"]]
    cmf_path <- paths[["cmf"]]
    .cli_action(action = "inform",
                msg = "{.arg terminal_run} mode has been selected enabling 
                model runs outside of your R IDE or R script. The following 
                steps are necessary to solve the model and parse outputs.")
    
    cli::cli_ol(items = c("Run the following command at your OS terminal: 
                {.val {exec_cmd}}",
                          "If errors are present in the terminal output during 
                          an ongoing run, it is possible to stop the relevant 
                          {.val {hsl}} process early according to your 
                          OS-specific system activity monitor.",
                          "Error and singularity indicators will be present in 
                          the model diagnostic output: {.val {diag_out}}.",
                          "If no errors or singularities are detected, run the 
                          following command to convert binary outputs: 
                          {.val {sol_parse_cmd}}.",
                          "The {.arg cmf_path} path to use as a value within 
                          {.fun teems::teems_parse} is {.path {cmf_path}}."))
    #clipr::write_clip(content = cmd)
    return()
  }
  
  cmd <- list(exec = exec_cmd,
              sol_parse = sol_parse_cmd)
  
  return(cmd)
}