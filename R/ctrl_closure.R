#' @importFrom rlang expr enquos current_env
#' @importFrom purrr pluck list_flatten
#' @importFrom targets tar_target_raw tar_cue
#' 
#' @keywords internal
#' @noRd
.closure_control <- function(config,
                             var_omit,
                             write_dir,
                             model_name) {

  t_closure_call <- rlang::expr(targets::tar_target_raw(
    name = "closure_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))
  
  # track closure file and any changes
  t_closure.file <- rlang::expr(targets::tar_target_raw(
    name = "closure_file",
    command = quote(expr = !!config[["closure_file"]]),
    format = "file"
  ))
  
  t_closure <- rlang::expr(targets::tar_target_raw(
    name = "closure",
    command = expression(expr = {
      closure <- tail(head(readLines(con = closure_file), -3), -1)
      return(closure)
    })
  ))

  t_check.closure <- rlang::expr(targets::tar_target_raw(
    name = "checked.closure",
    command = expression(.check_closure(
      closure = closure,
      sets = final.set_tib
    ))
  ))

  t_expand.closure <- rlang::expr(targets::tar_target_raw(
    name = "expanded.closure",
    command = expression(.expand_closure(
      closure = checked.closure,
      var_omit = !!var_omit,
      var_extract = tab_comp[["var_extract"]],
      sets = final.set_tib
    ))
  ))

  t_swap.in.closure <- rlang::expr(targets::tar_target_raw(
    name = "swapped.in.cls",
    command = expression(.swap_in(
      closure = expanded.closure,
      swap_in = !!config[["swap_in"]],
      sets = final.set_tib,
      var_extract = tab_comp[["var_extract"]]
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  t_swap.out.closure <- rlang::expr(targets::tar_target_raw(
    name = "swapped.out.cls",
    command = expression(.swap_out(
      closure = swapped.in.cls,
      swap_out = !!config[["swap_out"]],
      sets = final.set_tib,
      var_extract = tab_comp[["var_extract"]]
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  t_final.closure <- rlang::expr(targets::tar_target_raw(
    name = "final.closure",
    command = expression(.format_closure(
      closure = swapped.out.cls[["orig_closure"]]
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  # Write closure
  t_write.closure <- rlang::expr(targets::tar_target_raw(
    name = "write.closure",
    command = expression(.TEEMS_write(
      input = final.closure,
      file = paste0(!!model_name, ".cls"),
      write_object = "closure",
      write_dir = !!write_dir
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  ##############################################################################
  # gather and check all generated targets
  envir <- rlang::current_env()
  targets <- .gather_targets(criteria = "t_",
                             envir = envir)
  return(targets)
}
