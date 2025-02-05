#' Parameter Specifications
#'
#' This function manages the specification of parameters for model execution,
#' including handling intertemporal parameters, aggregating parameter data, and
#' preparing parameter outputs for both tablo and command file generation. It
#' utilizes the `targets` package to create a reproducible pipeline for tracking
#' changes, processing parameter data, and preparing model inputs and outputs.
#'
#' @inheritParams teems_parameters
#' @inheritParams teems_model
#' @param config A list of model configuration options generated from
#'   `teems_parameters`.
#'
#' @importFrom rlang expr
#' @importFrom qs2 qd_read
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom tibble tibble
#' @return A list of all generated targets within the parameter specification
#'   process.
#' @keywords internal
#' @noRd
.param_config <- function(config,
                          metadata,
                          ndigits,
                          full_exclude,
                          write_dir) {
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
          header_rename = !!config[["header_rename"]],
          coefficient_rename = !!config[["coefficient_rename"]]
        ))
      ))
    } else if (identical(x = file_type, y = "qs2")) {
      t_aux_par_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_par_array",
        command = expression(qs2::qd_read(file = aux_par_file))
      ))
    }

    # convert binary har files to a list of arrays and append
    t_par_array <- rlang::expr(targets::tar_target_raw(
      name = "par_array",
      command = expression(.read_har(
        con = par_file,
        header_rename = !!config[["header_rename"]],
        coefficient_rename = !!config[["coefficient_rename"]],
        append = aux_par_array,
        full_exclude = !!full_exclude
      ))
    ))
  } else {
    # convert binary har files to a list of arrays
    t_par_array <- rlang::expr(targets::tar_target_raw(
      name = "par_array",
      command = expression(.read_har(
        con = par_file,
        header_rename = !!config[["header_rename"]],
        coefficient_rename = !!config[["coefficient_rename"]],
        full_exclude = !!full_exclude
      ))
    ))
  }

  # add int here
  t_par_mod <- rlang::expr(targets::tar_target_raw(
    name = "mod.par_array",
    command = expression(.modify_array(
      ls_array = par_array,
      database_version = !!metadata[["database_version"]],
      set_array = set_array,
      sets = final.set_tib,
      time_steps = time_steps
    ))
  ))

  t_par_setnames <- rlang::expr(targets::tar_target_raw(
    name = "final.par_array",
    command = expression(.update_set_names(
      ls_array = mod.par_array,
      coeff_extract = tablo_coeff,
      metadata = !!metadata
    ))
  ))

  
  # convert list of data arrays to structured data
  t_ls_par <- rlang::expr(targets::tar_target_raw(
    name = "ls_par",
    command = expression(.construct_dt(
      ls_array = final.par_array,
      metadata = !!metadata,
      coeff_extract = tablo_coeff,
      set_array = set_array
    ))
  ))

  if (metadata[["convert"]]) {
    t_ls_par <- rlang::expr(targets::tar_target_raw(
      name = "converted.ls_par",
      command = expression(.convert_data(
        data = ls_par,
        data_format = !!metadata[["data_format"]]
      ))
    ))

    # construct tibbles from metadata and dts for each data type
    t_init.par_tib <- rlang::expr(targets::tar_target_raw(
      name = "init.par_tib",
      command = expression(.build_tibble(
        ls_data = converted.ls_par,
        preagg_header_replace = !!config[["preagg_data"]],
        coeff_extract = tablo_coeff
      ))
    ))
  } else {
    # construct tibbles from metadata and dts for each data type
    t_init.par_tib <- rlang::expr(targets::tar_target_raw(
      name = "init.par_tib",
      command = expression(.build_tibble(
        ls_data = ls_par,
        preagg_header_replace = !!config[["preagg_data"]],
        coeff_extract = tablo_coeff
      ))
    ))
  }
  
  # parameter value calculation
  t_weighted.par_tib <- rlang::expr(targets::tar_target_raw(
    name = "weighted.par_tib",
    command = expression(
      .weight_param(
        weights = init.base_tib,
        RDLT = !!config[["RORDELTA"]],
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
      coeff_list = tablo_coeff,
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
  targets <- .gather_targets(criteria = "t_")
  return(targets)
}
