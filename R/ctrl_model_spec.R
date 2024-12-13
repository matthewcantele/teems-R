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

  if (grepl(pattern = "\\.tab", x = config[["tab_file"]])) {
  # User-provided condition
  # Tablo file identification and tracking -------------------------------------
  target_tab_path <- rlang::expr(targets::tar_target_raw(
    name = "tab_path",
    command = quote(expr = !!config[["tab_file"]])
  ))

  # track any changes to Tablo file
  target_tab_file <- rlang::expr(targets::tar_target_raw(
    name = "tracked_tab_file",
    command = quote(expr = tab_path),
    format = "file"
  ))

  # produce Tablo file extract
  target_tablo <- rlang::expr(targets::tar_target_raw(
    name = "parsed.tablo",
    command = expression(.parse_tablo(tab_file = tracked_tab_file))
  ))

  } else {
    target_internal_tab <- rlang::expr(targets::tar_target_raw(
      name = "internal_tab_file",
      command = quote(expr = !!config[["tab_file"]])
    ))

    # produce Tablo file extract
    target_tablo <- rlang::expr(targets::tar_target_raw(
      name = "parsed.tablo",
      command = expression(.parse_tablo(tab_file = internal_tab_file,
                                        data_format = !!config[["data_format"]]))
    ))
  }

  # checks that the file statements defined within have a destination
  # core files
  target_core_read_files <- rlang::expr(targets::tar_target_raw(
    name = "core_read_files",
    command = quote(expr = list(
      GTAPSETS = !!config[["sets_csv_name"]],
      GTAPDATA = !!config[["data_csv_name"]],
      GTAPPARM = !!config[["parameter_csv_name"]]
    ))
  ))

  if (identical(x = config[["temporal_dynamics"]], y = "intertemporal")) {
    # intertemporal files
    target_int_read_files <- rlang::expr(targets::tar_target_raw(
      name = "int_read_files",
      command = quote(expr = list(
        INTDATA = !!config[["int_data_csv_name"]],
        INTPARM = !!config[["int_parameter_csv_name"]]
      ))
    ))

    # combine
    target_read_files <- rlang::expr(targets::tar_target_raw(
      name = "read_files",
      command = quote(expr = c(core_read_files, int_read_files))
    ))
  } else {
    target_read_files <- rlang::expr(targets::tar_target_raw(
      name = "read_files",
      command = quote(expr = core_read_files)
    ))
  }

  target_tablo_files <- rlang::expr(targets::tar_target_raw(
    name = "tablo_files",
    command = expression(.tablo_files(
      parsed_tablo = parsed.tablo[["extract"]]
    ))
  ))

  target_tab_file_check <- rlang::expr(targets::tar_target_raw(
    name = "tab_file_check",
    command = expression(.check_files(
      file_inputs = read_files,
      tab_file_inputs = tablo_files
    ))
  ))

  # Append "Write" statements
  target_final.tablo <- rlang::expr(targets::tar_target_raw(
    name = "final.tablo",
    command = expression(.append_tablo(
      tab = parsed.tablo[["tab"]],
      coeff = tablo_coeff
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  # Write tab file
  target_write.tablo <- rlang::expr(targets::tar_target_raw(
    name = "write.tablo",
    command = expression(.TEEMS_write(
      input = final.tablo,
      file = parsed.tablo[["tab_file"]],
      write_object = "tab",
      write_dir = !!launchpad_dir
    )),
    format = "file",
    cue = targets::tar_cue(mode = "always")
  ))

  ##############################################################################
  # gather and check all generated targets
  targets <- .gather_targets(criteria = "target_")
  return(targets)
}
