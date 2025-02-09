#' Closure Specifications
#'
#' This function defines and manages the specification of model
#' closures, including the option for standard or custom closures, and
#' the ability to swap variables in and out of the closure. It
#' leverages the `targets` package to create a reproducible pipeline
#' for specifying closures, ensuring that changes are tracked and
#' managed efficiently.
#'
#' @inheritParams teems_model
#' @inheritParams teems_deploy
#'
#' @param config A list of model configuration options generated from
#'   `teems_closure`.
#'
#' @importFrom rlang expr enquos
#' @importFrom purrr pluck list_flatten
#' @importFrom targets tar_target_raw tar_cue
#' @return A list of all generated targets within the closure
#'   specification process.
#' @keywords internal
#' @noRd
.closure_config <- function(config,
                            tab_file,
                            write_dir,
                            model_name) {

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

  t_tablo_var <- rlang::expr(targets::tar_target_raw(
    name = "tablo_var",
    command = expression(.tablo_variables(
      parsed_tablo = parsed.tablo[["extract"]]
    ))
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
      var_extract = tablo_var,
      sets = final.set_tib
    ))
  ))

  t_swap.in.closure <- rlang::expr(targets::tar_target_raw(
    name = "swapped.in.cls",
    command = expression(.swap_in(
      closure = expanded.closure,
      swap_in = !!config[["swap_in"]],
      sets = final.set_tib,
      var_extract = tablo_var
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  t_swap.out.closure <- rlang::expr(targets::tar_target_raw(
    name = "swapped.out.cls",
    command = expression(.swap_out(
      closure = swapped.in.cls,
      swap_out = !!config[["swap_out"]],
      sets = final.set_tib,
      var_extract = tablo_var
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
  targets <- .gather_targets(criteria = "t_")
  return(targets)
}
