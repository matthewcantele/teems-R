#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#'
#' @keywords internal
#' @noRd
.set_config <- function(config,
                        data_type,
                        metadata,
                        full_exclude,
                        write_dir) {
  
  t_set_call <- rlang::expr(targets::tar_target_raw(
    name = "set_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))

  # track any changes to designated har files
  t_set_file <- rlang::expr(targets::tar_target_raw(
    name = "set_file",
    command = quote(expr = !!config[["set_har"]]),
    format = "file"
  ))
  
  # file tracking on user-provided set mappings
  t_region_mapping <- rlang::expr(targets::tar_target_raw(
    name = "region_mapping",
    command = quote(expr = !!config[["region_mapping"]]),
    format = "file"
  ))
  
  t_sector_mapping <- rlang::expr(targets::tar_target_raw(
    name = "sector_mapping",
    command = quote(expr = !!config[["sector_mapping"]]),
    format = "file"
  ))
  
  t_endowment_mapping <- rlang::expr(targets::tar_target_raw(
    name = "endowment_mapping",
    command = quote(expr = !!config[["endowment_mapping"]]),
    format = "file"
  ))
  t_time_steps <- rlang::expr(targets::tar_target_raw(
    name = "time_steps",
    command = quote(expr = !!config[["time_steps"]])
  ))

  # convert binary har files to a list of arrays
  t_set_array <- rlang::expr(targets::tar_target_raw(
    name = "set_array",
    command = expression(.read_har(
      con = set_file,
      data_type = !!data_type,
      header_rename = NULL,
      coefficient_rename = NULL,
      full_exclude = !!full_exclude,
      call = set_call
    ))
  ))

  t_set_mod.set_array <- rlang::expr(targets::tar_target_raw(
    name = "mod.set_array",
    command = expression(.modify_array(
      ls_array = set_array,
      coeff_extract = coeff_extract,
      metadata = !!metadata,
      data_type = !!data_type
    ))
  ))

  # convert list of data arrays to structured data
  t_ls_set <- rlang::expr(targets::tar_target_raw(
    name = "ls_set",
    command = expression(.construct_dt(
      ls_array = mod.set_array,
      metadata = !!metadata,
      data_type = !!data_type,
      coeff_extract = tablo_sets[["sets"]]
    ))
  ))
  
  # construct tibbles from metadata and dts for each data type
  t_init.set_tib <- rlang::expr(targets::tar_target_raw(
    name = "init.set_tib",
    command = expression(.build_tibble(
      ls_data = ls_set,
      preagg_header_replace = NULL,
      coeff_extract = tablo_sets[["sets"]]
    ))
  ))

  t_set_mappings <- rlang::expr(targets::tar_target_raw(
    name = "set_mappings",
    command = expression(.retrieve_mappings(
      region_mapping = !!config[["region_mapping"]],
      sector_mapping = !!config[["sector_mapping"]],
      endowment_mapping = !!config[["endowment_mapping"]],
      data_format = !!metadata[["model_version"]]
    )),
    cue = targets::tar_cue(mode = "always")
  ))

  t_tablo_sets <- rlang::expr(targets::tar_target_raw(
    name = "tablo_sets",
    command = expression(.tablo_sets(
      parsed_tablo = parsed.tablo[["extract"]]
    ))
  ))

  t_precheck.set_mappings <- rlang::expr(targets::tar_target_raw(
    name = "precheck.set_mappings",
    command = expression(.retrieve_sets(
      set_mappings = set_mappings,
      nonint_sets = init.set_tib,
      model_sets = tablo_sets[["sets"]],
      database_version = !!metadata[["database_version"]],
      data_format = !!metadata[["model_version"]]
    ))
  ))

  t_checked.set_mappings <- rlang::expr(targets::tar_target_raw(
    name = "checked.set_mappings",
    command = expression(.check_mapping(
      model_sets = precheck.set_mappings,
      full_sets = init.set_tib
    ))
  ))

  # Expand sets according to Tablo specifications ------------------------------
  # Check set integrity
  # Set expansion
  t_final.set_tib <- rlang::expr(targets::tar_target_raw(
    name = "final.set_tib",
    command = expression(.expand_sets(
      sets = checked.set_mappings,
      time_steps = time_steps,
      set_extract = tablo_sets[["sets"]]
    ))
  ))

  # Write sets
  t_write.sets <- rlang::expr(targets::tar_target_raw(
    name = "write.set",
    command = expression(.write_sets(
      sets = final.set_tib,
      out_dir = !!write_dir
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
