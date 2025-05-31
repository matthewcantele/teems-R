#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#'
#' @keywords internal
#' @noRd
.set_control <- function(config,
                         set_map_files,
                         n_timestep,
                         data_type,
                         full_exclude,
                         write_dir) {
  t_set_call <- rlang::expr(targets::tar_target_raw(
    name = "set_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))
  
  t_set_file <- rlang::expr(targets::tar_target_raw(
    name = "set_file",
    command = quote(expr = !!config[["set_input"]]),
    format = "file"
  ))
  
  set_file_type <- attr(x = config[["set_input"]], which = "file_ext")
  
  if (identical(x = set_file_type, y = "har")) {
    # convert binary har files to a list of arrays
    t_set_array <- rlang::expr(targets::tar_target_raw(
      name = "set_array",
      command = expression(.read_har(
        con = set_file,
        data_type = "set",
        call = set_call
      ))
    ))
  } else if (identical(x = set_file_type, y = "qs2")) {
    t_set_array <- rlang::expr(targets::tar_target_raw(
      name = "set_array",
      command = expression(qs2::qs_read(file = set_file))
    ))
  }
  
  if (!is.null(x = config[["aux_set_file"]])) {
    t_aux_set_file <- rlang::expr(targets::tar_target_raw(
      name = "aux_set_file",
      command = quote(expr = !!config[["aux_set_file"]]),
      format = "file"
    ))

    file_type <- attr(x = config[["aux_set_file"]], "file_ext")

    if (identical(x = file_type, y = "har")) {
      t_aux_set_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_set_array",
        command = expression(.read_har(
          con = aux_set_file,
          call = set_call
        ))
      ))
    } else if (identical(x = file_type, y = "qs2")) {
      t_aux_set_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_set_array",
        command = expression(qs2::qs_read(file = aux_set_file))
      ))
    }
  } else {
    t_aux_set_array <- rlang::expr(targets::tar_target_raw(
      name = "aux_set_array",
      command = quote(expr = NULL)
    ))
  }

  t_set_mod.set_array <- rlang::expr(targets::tar_target_raw(
    name = "mod.set_array",
    command = expression(.modify_sets(
      set_array = set_array,
      append = aux_set_array,
      full_exclude = !!full_exclude,
      call = set_call
    ))
  ))

  t_set_map_files <- rlang::expr(targets::tar_target_raw(
    name = "set_map_files",
    command = quote(expr = !!set_map_files)
  ))

  t_set_map_file_inputs <- rlang::expr(targets::tar_target_raw(
    name = "set_map_file_inputs",
    command = quote(expr = set_map_files),
    pattern = quote(expr = map(set_map_files)),
    format = "file"
  ))

  t_set_map_names <- rlang::expr(targets::tar_target_raw(
    name = "set_map_file_input_names",
    command = expression(names(x = set_map_files))
  ))

  # construct tibbles from metadata and dts for each data type
  t_init.set_tib <- rlang::expr(targets::tar_target_raw(
    name = "init.set_tib",
    command = expression(.build_tibble(
      ls_array = mod.set_array,
      is_set_array = TRUE,
      set_extract = tab_comp[["set_extract"]][["sets"]]
    ))
  ))

  # Expand sets according to Tablo specifications ------------------------------
  # Check set integrity
  # Set expansion
  t_final.set_tib <- rlang::expr(targets::tar_target_raw(
    name = "expanded.set_tib",
    command = expression(.expand_sets(
      sets = init.set_tib,
      n_timestep = !!n_timestep,
      set_extract = tab_comp[["set_extract"]][["sets"]]
    ))
  ))

  t_precheck.set_mappings <- rlang::expr(targets::tar_target_raw(
    name = "final.set_tib",
    command = expression(.retrieve_mappings(
      set_map_file_inputs = set_map_file_inputs,
      set_map_file_input_names = set_map_file_input_names,
      sets = expanded.set_tib,
      database_version = metadata[["database_version"]],
      data_format = metadata[["data_format"]]
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
  targets <- .gather_targets(
    criteria = "t_",
    envir = envir
  )
  return(targets)
}