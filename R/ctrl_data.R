#' @importFrom rlang expr current_env
#' @importFrom qs2 qd_read
#' @importFrom targets tar_target_raw tar_cue
#'
#' @keywords internal
#' @noRd
.data_control <- function(config,
                          ndigits,
                          full_exclude,
                          write_dir) {
  t_data_call <- rlang::expr(targets::tar_target_raw(
    name = "data_call",
    command = rlang::expr(quote(expr = !!config[["call"]]))
  ))

  # track any changes to designated har files
  t_dat_file <- rlang::expr(targets::tar_target_raw(
    name = "dat_file",
    command = quote(expr = !!config[["dat_input"]]),
    format = "file"
  ))

# if these can be made into a function...
  dat_file_type <- attr(x = config[["dat_input"]], which = "file_ext")

  if (identical(x = dat_file_type, y = "har")) {
    # convert binary har files to a list of arrays
    t_dat_array <- rlang::expr(targets::tar_target_raw(
      name = "dat_array",
      command = expression(.read_har(
        con = dat_file,
        data_type = "dat",
        call = data_call
      ))
    ))
  } else if (identical(x = dat_file_type, y = "qs2")) {
    t_dat_array <- rlang::expr(targets::tar_target_raw(
      name = "dat_array",
      command = expression(qs2::qs_read(file = dat_file))
    ))
  }

  t_par_file <- rlang::expr(targets::tar_target_raw(
    name = "par_file",
    command = quote(expr = !!config[["par_input"]]),
    format = "file"
  ))

  par_file_type <- attr(x = config[["par_input"]], which = "file_ext")

  if (identical(x = par_file_type, y = "har")) {
    # convert binary har files to a list of arrays
    t_par_array <- rlang::expr(targets::tar_target_raw(
      name = "par_array",
      command = expression(.read_har(
        con = par_file,
        data_type = "par",
        call = data_call
      ))
    ))
  } else if (identical(x = par_file_type, y = "qs2")) {
    t_par_array <- rlang::expr(targets::tar_target_raw(
      name = "par_array",
      command = expression(qs2::qs_read(file = par_file))
    ))
  }
# file tracking on qs2?
  if (!is.null(x = config[["aux_input"]])) {
    aux_file_type <- attr(x = config[["aux_input"]], which = "file_ext")
    t_aux_file <- rlang::expr(targets::tar_target_raw(
      name = "aux_file",
      command = quote(expr = !!config[["aux_input"]]),
      format = "file"
    ))
    if (identical(x = aux_file_type, y = "har")) {
      # convert binary har files to a list of arrays
      t_aux_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_array",
        command = expression(.read_har(
          con = aux_file,
          call = data_call
        ))
      ))
    } else if (identical(x = aux_file_type, y = "qs2")) {
      t_aux_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_array",
        command = expression(qs2::qs_read(file = aux_file))
      ))
    }
  } else {
    t_aux_array <- rlang::expr(targets::tar_target_raw(
      name = "aux_array",
      command = quote(expr = NULL)
    ))
  }

  t_data_mod <- rlang::expr(targets::tar_target_raw(
    name = "mod.data_array",
    command = expression(.modify_array(
      dat_array = dat_array,
      par_array = par_array,
      aux_array = aux_array,
      set_array = mod.set_array,
      metadata = metadata,
      full_exclude = !!full_exclude,
      coeff_extract = tab_comp[["coeff_extract"]],
      call = data_call
    ))
  ))

  t_data_setnames <- rlang::expr(targets::tar_target_raw(
    name = "final.data_array",
    command = expression(.update_set_names(
      ls_array = mod.data_array,
      coeff_extract = tab_comp[["coeff_extract"]],
      data_format = metadata[["data_format"]],
      set_extract = tab_comp[["set_extract"]][["sets"]]
    ))
  ))

  # construct tibbles from metadata and dts for each data type
  t_init.data_tib <- rlang::expr(targets::tar_target_raw(
    name = "init.data_tib",
    command = expression(.build_tibble(
      ls_array = final.data_array,
      set_extract = tab_comp[["set_extract"]][["sets"]],
      int_sets = int_sets,
      unaggregated_input = !!config[["unaggregated_input"]],
      coeff_extract = tab_comp[["coeff_extract"]]
    ))
  ))

  # parameter value calculation
  t_weighted.data_tib <- rlang::expr(targets::tar_target_raw(
    name = "weighted.data_tib",
    command = expression(
      .weight_param(
        tib_data = init.data_tib,
        data_format = metadata[["data_format"]]
      )
    )
  ))

  # remove unnecessary headers and map new sets
  t_prepped.data_tib <- rlang::expr(targets::tar_target_raw(
    name = "prepped.data_tib",
    command = expression(.prep_data(
      tib_data = weighted.data_tib,
      sets = final.set_tib,
      coeff_extract = tab_comp[["coeff_extract"]]
    ))
  ))

  # aggregate to new sets
  t_aggregated.data_tib <- rlang::expr(targets::tar_target_raw(
    name = "aggregated.data_tib",
    command = expression(.aggregate_data(
      tib_data = prepped.data_tib,
      sets = final.set_tib
    ))
  ))

  # finalize data before write
  t_final.data_tib <- rlang::expr(targets::tar_target_raw(
    name = "final.data_tib",
    command = expression(.finalize_data(
      tib_data = aggregated.data_tib,
      aggregated_input = !!config[["aggregated_input"]],
      sets = final.set_tib
    ))
  ))

  # write data
  t_write.data <- rlang::expr(targets::tar_target_raw(
    name = "write.data",
    command = expression(.ragged_write(
      dat = final.data_tib,
      out_dir = !!write_dir,
      ndigits = !!ndigits
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