#' @keywords internal
#' @noRd
.construct_cmd <- function(paths,
                           terminal_run,
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
  #system("docker run --rm --mount type=bind,src=/home/mpc/teems_runs/null/GTAP-INTv1_v10/launchpad,dst=/home/launchpad teems:beta /bin/bash -c \"/home/teems-solver/lib/mpi/bin/mpiexec -n 1 /home/teems-solver/solver/hsl -cmdfile /home/launchpad/GTAP-INTv1_v10.cmf -matsol 0 -step1 2 -step2 4 -step3 8 -regset REG -enable_time -nsubints 1 -solmed Mmid -nesteddbbd 0  -presol 1 -laA 300 -laDi 500 -laD 200 -maxthreads 1 | tee /home/launchpad/out/solver_out_1736.txt\"")

  docker_preamble <- paste(
    "docker run --rm --mount",
    paste("type=bind", paste0("src=", paths[["run"]]), "dst=/opt/launchpad", sep = ","),
    paste0("teems", ":", .o_docker_tag()),
    "/bin/bash -c"
  )
  exec_preamble <- paste(
    docker_preamble,
    '"/opt/teems-solver/lib/mpi/bin/mpiexec',
    "-n", n_tasks,
    "/opt/teems-solver/solver/hsl",
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
    paste("-maxthreads", 1),
    "-nox",
    "| tee",
    paste0(docker_diagnostic_out, '"')
  )
  exec_cmd <- paste(exec_preamble, solver_param)
  sol_parse_cmd <- paste(docker_preamble, '"make -C /opt/teems-parser"')
  if (terminal_run) {
    cat(exec_cmd, file = file.path(paths[["run"]], "model_exec.txt"))
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
    return(terminal_run)
  }
  cmd <- list(exec = exec_cmd,
              sol_parse = sol_parse_cmd)
  
  return(cmd)
}
