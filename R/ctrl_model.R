#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.model_control <- function(config,
                           metadata,
                           launchpad_dir) {

  t_model_call <- rlang::expr(targets::tar_target_raw(
    name = "model_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))

  t_model_metadata <- rlang::expr(targets::tar_target_raw(
    name = "metadata",
    command = quote(expr = !!metadata)
  ))
  
  # Tablo file identification and tracking
  t_tab_path <- rlang::expr(targets::tar_target_raw(
    name = "tab_path",
    command = quote(expr = !!config[["tab_file"]])
  ))
  
  t_tab_components <- rlang::expr(targets::tar_target_raw(
    name = "tab_comp",
    command = expression(.process_tablo(
      tab_file = tab_path,
      var_omit = !!config[["var_omit"]],
      call = model_call,
      quiet = TRUE
    ))
  ))
  
  # track any changes to Tablo file
  t_tab_file <- rlang::expr(targets::tar_target_raw(
    name = "tracked_tab_file",
    command = quote(expr = tab_path),
    format = "file"
  ))

  # Append "Write" statements
  t_final.tablo <- rlang::expr(targets::tar_target_raw(
    name = "final.tablo",
    command = expression(.append_tablo(
      tab = tab_comp[["conden_tab"]],
      coeff_extract = tab_comp[["coeff_extract"]],
      sets = final.set_tib
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  # Write tab file
  t_write.tablo_mod <- rlang::expr(targets::tar_target_raw(
    name = "write.tablo_mod",
    command = expression(.TEEMS_write(
      input = final.tablo,
      file = tab_path,
      prepend_file = "modified",
      write_object = "tabfile",
      write_dir = !!launchpad_dir
    )),
    cue = targets::tar_cue(mode = "always")
  ))
  
  t_write.tablo_orig <- rlang::expr(targets::tar_target_raw(
    name = "write.tablo_orig",
    command = expression(.TEEMS_write(
      input = tab_comp[["orig_tab"]],
      file = tab_path,
      prepend_file = "original",
      write_object = "tabfile",
      write_dir = !!launchpad_dir
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
