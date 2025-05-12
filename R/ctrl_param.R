#' @importFrom rlang expr current_env
#' @importFrom qs2 qd_read
#' @importFrom targets tar_target_raw tar_cue
#' 
#' @keywords internal
#' @noRd
.param_control <- function(config,
                           coeff_extract,
                           set_extract,
                           data_type,
                           metadata,
                           ndigits,
                           full_exclude,
                           write_dir) {
  t_par_call <- rlang::expr(targets::tar_target_raw(
    name = "par_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))
  
  # track any changes to designated har files
  t_par_file <- rlang::expr(targets::tar_target_raw(
    name = "par_file",
    command = quote(expr = !!config[["par_har"]]),
    format = "file"
  ))
  
  if (!is.na(x = config[["aux_par"]])) {
    t_aux_par_file <- rlang::expr(targets::tar_target_raw(
      name = "aux_par_file",
      command = quote(expr = !!config[["aux_par"]]),
      format = "file"
    ))
    
    file_type <- attr(x = config[["aux_par"]], "file_ext")
    
    if (identical(x = file_type, y = "har")) {
      t_aux_par_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_par_array",
        command = expression(.read_har(
          con = aux_par_file,
          data_type = !!data_type,
          call = par_call
        ))
      ))
    } else if (identical(x = file_type, y = "qs2")) {
      t_aux_par_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_par_array",
        command = expression(qs2::qd_read(file = aux_par_file))
      ))
    }
  } else {
    t_aux_par_array <- rlang::expr(targets::tar_target_raw(
      name = "aux_par_array",
      command = quote(expr = NULL)
    ))
  }
  
  # convert binary har files to a list of arrays
  t_par_array <- rlang::expr(targets::tar_target_raw(
    name = "par_array",
    command = expression(.read_har(
      con = par_file,
      data_type = !!data_type,
      call = par_call
    ))
  ))
  
  t_par_mod <- rlang::expr(targets::tar_target_raw(
    name = "mod.par_array",
    command = expression(.modify_array(
      ls_array = par_array,
      metadata = !!metadata,
      data_type = !!data_type,
      coeff_extract = !!coeff_extract,
      header_rename = !!config[["header_rename"]],
      coefficient_rename = !!config[["coefficient_rename"]],
      full_exclude = !!full_exclude,
      set_array = set_array,
      sets = final.set_tib,
      time_steps = time_steps,
      append = aux_par_array,
      call = par_call
    ))
  ))

  t_par_setnames <- rlang::expr(targets::tar_target_raw(
    name = "final.par_array",
    command = expression(.update_set_names(
      ls_array = mod.par_array,
      coeff_extract = !!coeff_extract,
      metadata = !!metadata,
      data_type = !!data_type
    ))
  ))

  # convert list of data arrays to structured data
  t_ls_par <- rlang::expr(targets::tar_target_raw(
    name = "ls_par",
    command = expression(.construct_dt(
      ls_array = final.par_array,
      metadata = !!metadata,
      data_type = !!data_type,
      set_extract = !!set_extract[["sets"]]
    ))
  ))

    # construct tibbles from metadata and dts for each data type
    t_init.par_tib <- rlang::expr(targets::tar_target_raw(
      name = "init.par_tib",
      command = expression(.build_tibble(
        ls_data = ls_par,
        preagg_header_replace = !!config[["preagg_data"]],
        comp_extract = !!coeff_extract
      ))
    ))

  
  # parameter value calculation
  t_weighted.par_tib <- rlang::expr(targets::tar_target_raw(
    name = "weighted.par_tib",
    command = expression(
      .weight_param(
        weights = init.base_tib,
        par = init.par_tib,
        data_format = !!metadata[["model_version"]]
      )
    )
  ))

  # remove unnecessary headers and map new sets
  t_prepped.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "prepped.par_tib",
    command = expression(.prep_data(
      data_type = "par",
      tib_data = weighted.par_tib,
      sets = final.set_tib,
      coeff_extract = !!coeff_extract,
      data_format = !!metadata[["model_version"]]
    ))
  ))

  # aggregate to new sets
  t_final.par_tib <- rlang::expr(targets::tar_target_raw(
    name = "final.par_tib",
    command = expression(.aggregate_data(
      data_type = "par",
      tib_data = prepped.par_tib,
      sets = final.set_tib,
      postagg_header_replace = !!config[["postagg_data"]]
    ))
  ))

  # write parameters
  t_write.par <- rlang::expr(targets::tar_target_raw(
    name = "write.par",
    command = expression(.ragged_write(
      dat = final.par_tib,
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
