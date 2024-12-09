#' Set Specifications
#'
#' Defines and manages the specification of sets for model execution, including
#' region, sector, margin mappings, and optionally capital goods and endowment
#' mappings. It supports intertemporal model configurations by allowing the
#' specification of time steps and intervals. Utilizes the `targets` package for
#' reproducible workflows, ensuring that set specifications are tracked and
#' managed efficiently.
#'
#' @inheritParams teems_model
#' @param config A list of model configuration options generated from
#'   `teems_sets`.
#' @param temporal_dynamics An internally generated parameter which evaluate to
#'   either `static` or `intertemporal` within \link{teems_model}.
#' @param write_dir A directory to write model input files to as determined
#'   within \link{teems_write}.
#'
#' @importFrom rlang expr
#' @importFrom targets tar_target_raw tar_cue
#' @return A list of all generated targets within the set specification process.
#' @keywords internal
#' @noRd
.set_config <- function(config,
                        temporal_dynamics,
                        full_exclude,
                        write_dir) {

  # track any changes to designated har files
  target_set_file <- rlang::expr(targets::tar_target_raw(
    name = "set_file",
    command = quote(expr = !!config[["set_har"]]),
    format = "file"
  ))

  # convert binary har files to a list of arrays
  target_set_array <- rlang::expr(targets::tar_target_raw(
    name = "set_array",
    command = expression(.read_har(
      con = set_file,
      header_rename = NULL,
      coefficient_rename = NULL
    ))
  ))

  # convert list of data arrays to structured data
  target_ls_set <- rlang::expr(targets::tar_target_raw(
    name = "ls_set",
    command = expression(.constructDT(
      ls_array = set_array,
      metadata = metadata,
      full_exclude = !!full_exclude
    ))
  ))

  # construct tibbles from metadata and dts for each data type
  target_init.set_tib <- rlang::expr(targets::tar_target_raw(
    name = "init.set_tib",
    command = expression(.build_tibble(
      ls_data = ls_set,
      preagg_header_replace = NULL
    ))
  ))

  # conditional file tracking on user-provided set mappings
  if (grepl(pattern = "\\.csv", x = config[["region_mapping"]])) {
    target_region_mapping <- rlang::expr(targets::tar_target_raw(
      name = "region_mapping",
      command = quote(expr = !!config[["region_mapping"]]),
      format = "file"
    ))
  } else {
    target_region_mapping <- rlang::expr(targets::tar_target_raw(
      name = "region_mapping",
      command = quote(expr = !!config[["region_mapping"]])
    ))
  }

  if (grepl(pattern = "\\.csv", x = config[["sector_mapping"]])) {
    target_sector_mapping <- rlang::expr(targets::tar_target_raw(
      name = "sector_mapping",
      command = quote(expr = !!config[["sector_mapping"]]),
      format = "file"
    ))
  } else {
    target_sector_mapping <- rlang::expr(targets::tar_target_raw(
      name = "sector_mapping",
      command = quote(expr = !!config[["sector_mapping"]])
    ))
  }

  if (grepl(pattern = "\\.csv", x = config[["endowment_mapping"]])) {
    target_endowment_mapping <- rlang::expr(targets::tar_target_raw(
      name = "endowment_mapping",
      command = quote(expr = !!config[["endowment_mapping"]]),
      format = "file"
    ))
  } else {
    target_endowment_mapping <- rlang::expr(targets::tar_target_raw(
      name = "endowment_mapping",
      command = quote(expr = !!config[["endowment_mapping"]])
    ))
  }

  target_set_mappings <- rlang::expr(targets::tar_target_raw(
    name = "set_mappings",
    command = expression(.retrieve_mappings(
      region_mapping = !!config[["region_mapping"]],
      sector_mapping = !!config[["sector_mapping"]],
      endowment_mapping = !!config[["endowment_mapping"]],
      data_format = metadata[["data_format"]]
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  target_tablo_sets <- rlang::expr(targets::tar_target_raw(
    name = "tablo_sets",
    command = expression(.tablo_sets(
      parsed_tablo = parsed.tablo[["extract"]]
    ))
  ))

  target_precheck.set_mappings <- rlang::expr(targets::tar_target_raw(
    name = "precheck.set_mappings",
    command = expression(.retrieve_sets(
      set_mappings = set_mappings,
      nonint_sets = init.set_tib,
      model_sets = tablo_sets[["sets"]],
      database_version = metadata[["database_version"]]
    ))
  ))

  target_checked.set_mappings <- rlang::expr(targets::tar_target_raw(
    name = "checked.set_mappings",
    command = expression(.check_mapping(
      model_sets = precheck.set_mappings,
      full_sets = init.set_tib
    ))
  ))

  # Expand sets according to Tablo specifications ------------------------------
  # Check set integrity
  # Set expansion
  target_final.set_tib <- rlang::expr(targets::tar_target_raw(
    name = "final.set_tib",
    command = expression(.expand_sets(
      nonint_sets = checked.set_mappings,
      int_sets = time_sets,
      set_extract = tablo_sets[["sets"]]
    ))
  ))

  # Write sets
  target_write.sets <- rlang::expr(targets::tar_target_raw(
    name = "write.sets",
    command = expression(.write_sets(
      sets = final.set_tib,
      file_name = tab_file_check[["GTAPSETS"]],
      out_dir = !!write_dir
    )),
    format = "file"
  ))

  ##############################################################################
  # gather and check all generated targets
  targets <- .gather_targets(criteria = "target_")
  return(targets)
}
