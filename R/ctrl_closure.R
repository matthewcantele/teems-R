#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#' 
#' @keywords internal
#' @noRd
.closure_control <- function(closure_file,
                             swap_in,
                             swap_out,
                             var_omit,
                             write_dir,
                             model_name) {
  t_closure.file <- rlang::expr(targets::tar_target_raw(
    "closure_file",
    quote(!!closure_file),
    format = "file"
  ))

  t_closure <- rlang::expr(targets::tar_target_raw(
    "closure",
    expression({
      closure <- tail(head(readLines(closure_file), -3), -1)
      return(closure)
    })
  ))

  t_prep.closure <- rlang::expr(targets::tar_target_raw(
    "prepped.closure",
    expression(.prep_closure(
      closure = closure,
      var_omit = !!var_omit,
      var_extract = tab_comp$var_extract,
      sets = final.set_tib
    ))
  ))

  t_swap.in.closure <- rlang::expr(targets::tar_target_raw(
    "swapped.in.cls",
    expression(.swap_in(
      closure = prepped.closure,
      swap_in = !!swap_in,
      sets = final.set_tib,
      var_extract = tab_comp$var_extract
    ))
  ))

  t_swap.out.closure <- rlang::expr(targets::tar_target_raw(
    "swapped.out.cls",
    expression(.swap_out(
      closure = swapped.in.cls,
      swap_out = !!swap_out,
      sets = final.set_tib,
      var_extract = tab_comp$var_extract
    ))
  ))

  t_final.closure <- rlang::expr(targets::tar_target_raw(
    "final.closure",
    expression(.format_closure(
      closure = swapped.out.cls$orig_closure
    )),
    cue = targets::tar_cue("always")
  ))

  # Write closure
  t_write.closure <- rlang::expr(targets::tar_target_raw(
    "write.closure",
    expression(.TEEMS_write(
      input = final.closure,
      file = paste0(!!model_name, ".cls"),
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
