#' Model Specifications
#'
#' This function defines and manages the workflow for general model
#' specifications. It leverages the `targets` package to create a reproducible
#' pipeline that tracks changes, processes data, and prepares outputs for
#' modeling.
#'
#' @inheritParams teems_model
#' @param config A list of model configuration options.
#' @param launchpad_dir A directory to write all input files to generated from
#'   `teems_write`.
#'
#' @importFrom rlang expr
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom purrr pluck
#' @return A list of all generated targets within the general model
#'   specification process.
#' @keywords internal
#' @noRd
.model_config <- function(config,
                          launchpad_dir) {

  # Tablo file identification and tracking -------------------------------------
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
                                      model_version = !!config[["model_version"]]))
  ))

  t_tablo_files <- rlang::expr(targets::tar_target_raw(
    name = "tablo_files",
    command = expression(.tablo_files(
      parsed_tablo = parsed.tablo[["extract"]]
    ))
  ))

  # Append "Write" statements
  t_final.tablo <- rlang::expr(targets::tar_target_raw(
    name = "final.tablo",
    command = expression(.append_tablo(
      tab = parsed.tablo[["tab"]],
      coeff = tablo_coeff
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
  targets <- .gather_targets(criteria = "t_")
  return(targets)
}
