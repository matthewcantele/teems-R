#' @importFrom rlang expr current_env
#' @importFrom targets tar_target_raw tar_cue
#'
#' @keywords internal
#' @noRd
.set_control <- function(set_input,
                         set_map_files,
                         data_type,
                         write_dir) {
  t_set_file <- rlang::expr(targets::tar_target_raw(
    "set_file",
    quote(!!set_input),
    format = "file"
  ))

  set_file_type <- attr(set_input, "file_ext")

  if (set_file_type %=% "har") {
    t_set_array <- rlang::expr(targets::tar_target_raw(
      "set_array",
      expression(.read_har(
        con = set_file,
        data_type = "set"
      ))
    ))
  } else if (set_file_type %=% "qs2") {
    t_set_array <- rlang::expr(targets::tar_target_raw(
      "set_array",
      expression(qs2::qs_read(file = set_file))
    ))
  }

  # if (!is.null(config$aux_set_file)) {
  #   t_aux_set_file <- rlang::expr(targets::tar_target_raw(
  #     "aux_set_file",
  #     quote(!!config$aux_set_file),
  #     format = "file"
  #   ))
  # 
  #   file_type <- attr(config$aux_set_file, "file_ext")
  # 
  #   if (file_type %=% "har") {
  #     t_aux_set_array <- rlang::expr(targets::tar_target_raw(
  #       "aux_set_array",
  #       expression(.read_har(
  #         con = aux_set_file
  #       ))
  #     ))
  #   } else if (file_type %=% "qs2") {
  #     t_aux_set_array <- rlang::expr(targets::tar_target_raw(
  #       "aux_set_array",
  #       expression(qs2::qs_read(file = aux_set_file))
  #     ))
  #   }
  # } else {
  #   t_aux_set_array <- rlang::expr(targets::tar_target_raw(
  #     "aux_set_array",
  #     quote(NULL)
  #   ))
  # }

  # if (!isTRUE(is.na(int_data))) {
  # t_int_sets <- rlang::expr(targets::tar_target_raw(
  #   "int_sets",
  #   expression(.build_int_sets(
  #     set_extract = tab_comp$set_extract$sets,
  #     int_data = !!int_data,
  #     reference_year = metadata$reference_year
  #   ))
  # ))
  # } else {
  #   t_int_sets <- rlang::expr(targets::tar_target_raw(
  #     "int_sets",
  #     quote(NA)
  #     ))
  # }

  t_set_mod.set_array <- rlang::expr(targets::tar_target_raw(
    "mod.set_array",
    expression(.modify_sets(
      set_array = set_array,
      append = NULL,
      set_extract = purrr::pluck(tab_comp, "set_extract", "sets")
    ))
  ))

  t_set_map_files <- rlang::expr(targets::tar_target_raw(
    "set_map_files",
    quote(!!set_map_files)
  ))

  t_set_map_file_inputs <- rlang::expr(targets::tar_target_raw(
    "set_map_file_inputs",
    quote(set_map_files),
    pattern = quote(map(set_map_files)),
    format = "file"
  ))

  t_set_map_names <- rlang::expr(targets::tar_target_raw(
    "set_map_file_input_names",
    expression(names(set_map_files))
  ))

  t_init.set_tib <- rlang::expr(targets::tar_target_raw(
    "init.set_tib",
    expression(.build_tibble(
      ls_ = mod.set_array,
      sets = purrr::pluck(tab_comp, "set_extract", "sets")
    ))
  ))

  t_final.set_tib <- rlang::expr(targets::tar_target_raw(
    "expanded.set_tib",
    expression(.expand_sets(
      sets = init.set_tib,
      set_extract = purrr::pluck(tab_comp, "set_extract", "sets")
    ))
  ))

  t_precheck.set_mappings <- rlang::expr(targets::tar_target_raw(
    "final.set_tib",
    expression(.retrieve_mappings(
      set_map_file_inputs = set_map_file_inputs,
      set_map_file_input_names = set_map_file_input_names,
      sets = expanded.set_tib,
      metadata = metadata,
      CYRS = par_array[[.o_timestep_header()]]
    ))
  ))

  t_write.sets <- rlang::expr(targets::tar_target_raw(
    "write.set",
    expression(.write_sets(
      sets = final.set_tib,
      out_dir = !!write_dir
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