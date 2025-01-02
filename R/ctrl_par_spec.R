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
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom tibble tibble
#' @return A list of all generated targets within the parameter specification
#'   process.
#' @keywords internal
#' @noRd
.param_config <- function(config,
                          ndigits,
                          full_exclude,
                          write_dir) {

    # track any changes to designated har files
    target_par_file <- rlang::expr(targets::tar_target_raw(
      name = "par_file",
      command = quote(expr = !!config[["par_har"]]),
      format = "file"
    ))

    # convert binary har files to a list of arrays
    target_par_array <- rlang::expr(targets::tar_target_raw(
      name = "par_array",
      command = expression(.read_har(
        con = par_file,
        header_rename = !!config[["header_rename"]],
        coefficient_rename = !!config[["coefficient_rename"]]
      ))
    ))

    # convert list of data arrays to structured data
    target_ls_par <- rlang::expr(targets::tar_target_raw(
      name = "ls_par",
      command = expression(.construct_dt(
        ls_array = par_array,
        metadata = metadata,
        coeff_extract = tablo_coeff,
        full_exclude = !!full_exclude,
        set_array = set_array
      ))
    ))

    target_ls_par_mod <- rlang::expr(targets::tar_target_raw(
      name = "ls_par_mod",
      command = expression(.modify_data(
        ls_array = ls_par,
        metadata = metadata,
        aux_data = tablo_coeff
      ))
    ))

    # construct tibbles from metadata and dts for each data type
    target_init.par_tib <- rlang::expr(targets::tar_target_raw(
      name = "init.par_tib",
      command = expression(.build_tibble(
        ls_data = ls_par_mod,
        preagg_header_replace = !!config[["preagg_data"]]
      ))
    ))

    # parameter value calculation
    target_weighted.par_tib <- rlang::expr(targets::tar_target_raw(
      name = "weighted.par_tib",
      command = expression(
        .weight_param(
          weights = init.base_tib,
          RDLT = !!config[["RORDELTA"]],
          par = init.par_tib,
          data_format = metadata[["model_version"]]
        )
      )
    ))

    # remove unnecessary headers and map new sets
    target_prepped.base_tib <- rlang::expr(targets::tar_target_raw(
      name = "prepped.par_tib",
      command = expression(.prep_data(
        data_type = "par",
        tib_data = weighted.par_tib,
        sets = final.set_tib,
        coeff_list = tablo_coeff
      ))
    ))

    # aggregate to new sets
    target_final.par_tib <- rlang::expr(targets::tar_target_raw(
      name = "final.par_tib",
      command = expression(.aggregate_data(
        data_type = "par",
        tib_data = prepped.par_tib,
        sets = final.set_tib,
        postagg_header_replace = !!config[["postagg_data"]]
      ))
    ))

    # write parameters
    target_write.par <- rlang::expr(targets::tar_target_raw(
      name = "write.par",
      command = expression(.ragged_write(
        dat = final.par_tib,
        file_name = tab_file_check[["GTAPPARM"]],
        out_dir = !!write_dir,
        ndigits = !!ndigits
      )),
      format = "file"
    ))

  ##############################################################################
  # gather and check all generated targets
  targets <- .gather_targets(criteria = "target_")
  return(targets)
}
