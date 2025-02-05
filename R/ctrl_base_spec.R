#' Data Specifications
#'
#' This function manages the specification of data for model execution,
#' including handling time-related coefficients, aggregating base data,
#' incorporating external data sources, specifying output data configurations,
#' and preparing various outputs for both tablo and command file generation. It
#' utilizes the `targets` package to create a reproducible pipeline for tracking
#' changes, processing data, and preparing model inputs and outputs.
#'
#' @inheritParams teems_base
#' @inheritParams teems_model
#' @inheritParams teems_time
#'
#' @importFrom rlang expr
#' @importFrom qs2 qd_read
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom data.table fread
#' @return A list of all generated targets within the data specification process.
#' @keywords internal
#' @noRd
.base_config <- function(config,
                         metadata,
                         ndigits,
                         full_exclude,
                         write_dir) {
  # track any changes to designated har files
  t_base_file <- rlang::expr(targets::tar_target_raw(
    name = "base_file",
    command = quote(expr = !!config[["dat_har"]]),
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
          header_rename = !!config[["header_rename"]],
          coefficient_rename = !!config[["coefficient_rename"]]
        ))
      ))
    } else if (identical(x = file_type, y = "qs2")) {
      t_aux_base_array <- rlang::expr(targets::tar_target_raw(
        name = "aux_base_array",
        command = expression(qs2::qd_read(file = aux_base_file))
      ))
    }

    # convert binary har files to a list of arrays and append
    t_base_array <- rlang::expr(targets::tar_target_raw(
      name = "base_array",
      command = expression(.read_har(
        con = base_file,
        header_rename = !!config[["header_rename"]],
        coefficient_rename = !!config[["coefficient_rename"]],
        append = aux_base_array,
        full_exclude = !!full_exclude
      ))
    ))
  } else {
    # convert binary har files to a list of arrays
    t_base_array <- rlang::expr(targets::tar_target_raw(
      name = "base_array",
      command = expression(.read_har(
        con = base_file,
        header_rename = !!config[["header_rename"]],
        coefficient_rename = !!config[["coefficient_rename"]],
        full_exclude = !!full_exclude
      ))
    ))
  }

  t_base_mod <- rlang::expr(targets::tar_target_raw(
    name = "mod.base_array",
    command = expression(.modify_array(
      ls_array = base_array,
      database_version = !!metadata[["database_version"]],
      sets = final.set_tib,
      time_steps = time_steps,
      base_year = !!metadata[["reference_year"]]
    ))
  ))

  t_base_setnames <- rlang::expr(targets::tar_target_raw(
    name = "final.base_array",
    command = expression(.update_set_names(
      ls_array = mod.base_array,
      coeff_extract = tablo_coeff,
      metadata = !!metadata
    ))
  ))

  # extract coefficient information from tab file
  t_tablo_coeff <- rlang::expr(targets::tar_target_raw(
    name = "tablo_coeff",
    command = expression(.tablo_coeff(
      parsed_tablo = parsed.tablo[["extract"]]
    ))
  ))

  # convert list of data arrays to structured data
  t_ls_base <- rlang::expr(targets::tar_target_raw(
    name = "ls_base",
    command = expression(.construct_dt(
      ls_array = final.base_array,
      metadata = !!metadata,
      coeff_extract = tablo_coeff,
    ))
  ))
  
  if (metadata[["convert"]]) {
    t_ls_base <- rlang::expr(targets::tar_target_raw(
      name = "converted.ls_base",
      command = expression(.convert_data(
        data = ls_base,
        data_format = !!metadata[["data_format"]]
      ))
    ))
    
    # construct tibbles from metadata and dts for each data type
    t_init.base_tib <- rlang::expr(targets::tar_target_raw(
      name = "init.base_tib",
      command = expression(.build_tibble(
        ls_data = converted.ls_base,
        preagg_header_replace = !!config[["preagg_data"]],
        coeff_extract = tablo_coeff
      ))
    ))
  } else {
    # construct tibbles from metadata and dts for each data type
    t_init.base_tib <- rlang::expr(targets::tar_target_raw(
      name = "init.base_tib",
      command = expression(.build_tibble(
        ls_data = ls_base,
        preagg_header_replace = !!config[["preagg_data"]],
        coeff_extract = tablo_coeff
      ))
    ))
  }

  # remove unnecessary headers and map new sets
  t_prepped.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "prepped.base_tib",
    command = expression(.prep_data(
      data_type = "dat",
      tib_data = init.base_tib,
      sets = final.set_tib,
      coeff_list = tablo_coeff,
      data_format = !!metadata[["model_version"]]
    ))
  ))

  # aggregate to new sets
  t_final.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "final.base_tib",
    command = expression(.aggregate_data(
      data_type = "dat",
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
  targets <- .gather_targets(criteria = "t_")
  return(targets)
}