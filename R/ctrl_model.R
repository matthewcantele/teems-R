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
    "model_call",
    rlang::expr(quote(!!config$call))
  ))

  t_model_metadata <- rlang::expr(targets::tar_target_raw(
    "metadata",
    quote(!!metadata)
  ))

  t_tab_path <- rlang::expr(targets::tar_target_raw(
    "tab_path",
    quote(!!config$tab_file)
  ))
  
  t_var_omit <- rlang::expr(targets::tar_target_raw(
    "var_omit",
    quote(!!config$var_omit)
  ))

  t_tab_components <- rlang::expr(targets::tar_target_raw(
    "tab_comp",
    expression(.process_tablo(
      tab_file = tab_path,
      var_omit = var_omit,
      call = model_call
    ))
  ))

  t_tab_file <- rlang::expr(targets::tar_target_raw(
    "tracked_tab_file",
    quote(tab_path),
    format = "file"
  ))

  t_final.tablo <- rlang::expr(targets::tar_target_raw(
    "final.tablo",
    expression(.append_tablo(
      tab = tab_comp$conden_tab,
      coeff_extract = tab_comp$coeff_extract,
      sets = final.set_tib
    )),
    cue = targets::tar_cue("always")
  ))

  t_write.tablo_mod <- rlang::expr(targets::tar_target_raw(
    "write.tablo_mod",
    expression(.TEEMS_write(
      input = final.tablo,
      file = tab_path,
      write_dir = !!launchpad_dir,
      prepend_file = "modified"
    )),
    cue = targets::tar_cue("always")
  ))

  t_write.tablo_orig <- rlang::expr(targets::tar_target_raw(
    "write.tablo_orig",
    expression(.TEEMS_write(
      input = tab_comp$orig_tab,
      file = tab_path,
      write_dir = !!launchpad_dir,
      prepend_file = "original"
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
