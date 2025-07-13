#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#' 
#' @keywords internal
#' @noRd
.shock_control <- function(shock,
                           shock_file,
                           write_dir) {
  
  t_raw.shocks <- rlang::expr(targets::tar_target_raw(
    "raw.shocks",
    quote(!!shock),
    cue = targets::tar_cue("always")
  ))
  
  if (!is.null(shock_file)) {
    t_shock_file <- rlang::expr(targets::tar_target_raw(
      "shock_file",
      quote(!!shock_file),
      format = "file"
    ))

    t_loaded.shocks <- rlang::expr(targets::tar_target_raw(
      "loaded.shocks",
      expression(.usr_shk(shock_file = shock_file)),
      cue = targets::tar_cue("always")
    ))
  }

  if (!is.null(shock)) {
    t_loaded.shocks <- rlang::expr(targets::tar_target_raw(
      "loaded.shocks",
      expression(.shock_load(
        shocks = raw.shocks,
        closure = swapped.out.cls,
        sets = final.set_tib,
        reference_year = metadata$reference_year
      )),
      cue = targets::tar_cue("always")
    ))
  }

  if (is.null(shock) && is.null(shock_file)) {
    t_loaded.shocks <- rlang::expr(targets::tar_target_raw(
      "loaded.shocks",
      expression(.null_shk(shock = raw.shocks)),
      cue = targets::tar_cue("always")
    ))
  }

  t_write.shocks <- rlang::expr(targets::tar_target_raw(
    "write.shocks",
    expression(.TEEMS_write(
      input = loaded.shocks$shocks,
      file = loaded.shocks$shock_file,
      write_dir = !!write_dir
    )),
    cue = targets::tar_cue("always")
  ))

  envir <- rlang::current_env()
  targets <- .gather_targets(
    criteria = "t_",
    envir = envir
  )
  return(targets)
}