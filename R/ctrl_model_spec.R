#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.model_config <- function(config,
                          launchpad_dir) {

  t_model_call <- rlang::expr(targets::tar_target_raw(
    name = "model_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))
  
  # Tablo file identification and tracking
  t_tab_path <- rlang::expr(targets::tar_target_raw(
    name = "tab_path",
    command = quote(expr = !!config[["tab_file"]])
  ))

  # track any changes to Tablo file
  t_tab_file <- rlang::expr(targets::tar_target_raw(
    name = "tracked_tab_file",
    command = quote(expr = tab_path),
    format = "file"
  ))

  # produce Tablo file extract
  t_tablo <- rlang::expr(targets::tar_target_raw(
    name = "parsed.tablo",
    command = expression(.parse_tablo(tab_file = tracked_tab_file,
                                      var_omit = !!config[["var_omit"]],
                                      model_version = !!config[["model_version"]]))
  ))

  # extract var information from tab file
  t_tablo_var <- rlang::expr(targets::tar_target_raw(
    name = "tablo_var",
    command = expression(.tablo_variables(
      parsed_tablo = parsed.tablo[["extract"]],
      call = model_call
    ))
  ))
  
  # extract coefficient information from tab file
  t_coeff_extract <- rlang::expr(targets::tar_target_raw(
    name = "coeff_extract",
    command = expression(.tablo_coeff(
      parsed_tablo = parsed.tablo[["extract"]],
      call = model_call
    ))
  ))
  
  # Append "Write" statements
  t_final.tablo <- rlang::expr(targets::tar_target_raw(
    name = "final.tablo",
    command = expression(.append_tablo(
      tab = parsed.tablo[["tab"]],
      coeff_extract = coeff_extract,
      sets = final.set_tib
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  # Write tab file
  t_write.tablo <- rlang::expr(targets::tar_target_raw(
    name = "write.tablo",
    command = expression(.TEEMS_write(
      input = final.tablo,
      file = parsed.tablo[["tab_file"]],
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
