#' @importFrom rlang expr current_env
#' @importFrom qs2 qd_read
#' @importFrom targets tar_target_raw tar_cue
#'
#' @keywords internal
#' @noRd
.base_config <- function(config,
                         data_type,
                         metadata,
                         ndigits,
                         full_exclude,
                         write_dir) {
  
  t_base_call <- rlang::expr(targets::tar_target_raw(
    name = "base_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))

  # track any changes to designated har files
  t_base_file <- rlang::expr(targets::tar_target_raw(
    name = "base_file",
    command = quote(expr = !!config[["base_har"]]),
    format = "file"
  ))
  
  if (!is.na(x = config[["aux_base"]])) {
    t_aux_base_file <- rlang::expr(targets::tar_target_raw(
      name = "aux_base_file",
      command = quote(expr = !!config[["aux_base"]]),
      format = "file"
    ))

    file_type <- attr(x = config[["aux_base"]], "file_ext")

    if (identical(x = file_type, y = "har")) {
      t_aux_base_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_base_array",
        command = expression(.read_har(
          con = aux_base_file,
          data_type = !!data_type,
          call = base_call
        ))
      ))
    } else if (identical(x = file_type, y = "qs2")) {
      t_aux_base_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_base_array",
        command = expression(qs2::qd_read(file = aux_base_file))
      ))
    }
  } else {
    t_aux_base_array <- rlang::expr(targets::tar_target_raw(
      name = "aux_base_array",
      command = quote(expr = NULL)
    ))
  }
  
  # convert binary har files to a list of arrays
  t_base_array <- rlang::expr(targets::tar_target_raw(
    name = "base_array",
    command = expression(.read_har(
      con = base_file,
      data_type = !!data_type,
      call = base_call
    ))
  ))
  
  t_base_mod <- rlang::expr(targets::tar_target_raw(
    name = "mod.base_array",
    command = expression(.modify_array(
      ls_array = base_array,
      metadata = !!metadata,
      data_type = !!data_type,
      coeff_extract = coeff_extract,
      header_rename = !!config[["header_rename"]],
      coefficient_rename = !!config[["coefficient_rename"]],
      full_exclude = !!full_exclude,
      sets = final.set_tib,
      time_steps = time_steps,
      append = aux_base_array,
      call = base_call
    ))
  ))

  # if (!is.na(x = config[["aux_base"]])) {
  #   t_aux_base_file <- rlang::expr(targets::tar_target_raw(
  #     name = "aux_base_file",
  #     command = quote(expr = !!config[["aux_base"]]),
  #     format = "file"
  #   ))
  # 
  #   file_type <- attr(x = config[["aux_base"]], "file_ext")
  #   if (identical(x = file_type, y = "har")) {
  #     t_aux_base_array <- rlang::expr(targets::tar_target_raw(
  #       name = "aux_base_array",
  #       command = expression(.read_har(
  #         con = aux_base_file,
  #         data_type = !!data_type,
  #         coeff_extract = coeff_extract,
  #         header_rename = !!config[["header_rename"]],
  #         coefficient_rename = !!config[["coefficient_rename"]],
  #         full_exclude = !!full_exclude,
  #         call = base_call
  #       ))
  #     ))
  #   } else if (identical(x = file_type, y = "qs2")) {
  #     t_aux_base_array <- rlang::expr(targets::tar_target_raw(
  #       name = "aux_base_array",
  #       command = expression(qs2::qd_read(file = aux_base_file))
  #     ))
  #   }
  # 
  #   # convert binary har files to a list of arrays and append
  #   t_base_array <- rlang::expr(targets::tar_target_raw(
  #     name = "base_array",
  #     command = expression(.read_har(
  #       con = base_file,
  #       data_type = !!data_type,
  #       coeff_extract = coeff_extract,
  #       header_rename = !!config[["header_rename"]],
  #       coefficient_rename = !!config[["coefficient_rename"]],
  #       append = aux_base_array,
  #       full_exclude = !!full_exclude,
  #       call = base_call
  #     ))
  #   ))
  # } else {
  # 
  # }

  t_base_setnames <- rlang::expr(targets::tar_target_raw(
    name = "final.base_array",
    command = expression(.update_set_names(
      ls_array = mod.base_array,
      coeff_extract = coeff_extract,
      metadata = !!metadata,
      data_type = !!data_type
    ))
  ))

  # convert list of data arrays to structured data
  t_ls_base <- rlang::expr(targets::tar_target_raw(
    name = "ls_base",
    command = expression(.construct_dt(
      ls_array = final.base_array,
      metadata = !!metadata,
      data_type = !!data_type,
      coeff_extract = coeff_extract,
      sets = tablo_sets[["sets"]]
    ))
  ))

  # construct tibbles from metadata and dts for each data type
  t_init.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "init.base_tib",
    command = expression(.build_tibble(
      ls_data = ls_base,
      preagg_header_replace = !!config[["preagg_data"]],
      coeff_extract = coeff_extract
    ))
  ))

  # remove unnecessary headers and map new sets
  t_prepped.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "prepped.base_tib",
    command = expression(.prep_data(
      data_type = !!data_type,
      tib_data = init.base_tib,
      sets = final.set_tib,
      coeff_list = coeff_extract,
      data_format = !!metadata[["model_version"]]
    ))
  ))

  # aggregate to new sets
  t_final.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "final.base_tib",
    command = expression(.aggregate_data(
      data_type = !!data_type,
      tib_data = prepped.base_tib,
      sets = final.set_tib,
      postagg_header_replace = !!config[["postagg_data"]]
    ))
  ))

  # write data
  t_write.dat <- rlang::expr(targets::tar_target_raw(
    name = "write.base",
    command = expression(.ragged_write(
      dat = final.base_tib,
      out_dir = !!write_dir,
      ndigits = !!ndigits
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