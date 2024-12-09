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
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom data.table fread
#' @return A list of all generated targets within the data specification process.
#' @keywords internal
#' @noRd
.base_config <- function(config,
                         exclude_inactive,
                         ndigits,
                         full_exclude,
                         write_dir) {

  # track any changes to designated har files
  target_base_file <- rlang::expr(targets::tar_target_raw(
    name = "base_file",
    command = quote(expr = !!config[["dat_har"]]),
    format = "file"
  ))

  # convert binary har files to a list of arrays
  target_base_array <- rlang::expr(targets::tar_target_raw(
    name = "base_array",
    command = expression(.read_har(
      con = base_file,
      header_rename = !!config[["header_rename"]],
      coefficient_rename = !!config[["coefficient_rename"]]
    ))
  ))

  # extract metadata from dat headers
  target_branched_metadata <- rlang::expr(targets::tar_target_raw(
    name = "metadata",
    command = expression(.extract_metadata(
      ls_array = base_array
    ))
  ))

  # extract coefficient information from tab file
  target_tablo_coeff <- rlang::expr(targets::tar_target_raw(
    name = "tablo_coeff",
    command = expression(.tablo_coeff(
      parsed_tablo = parsed.tablo[["extract"]]
    ))
  ))

  # convert list of data arrays to structured data
  target_ls_dat <- rlang::expr(targets::tar_target_raw(
    name = "ls_dat",
    command = expression(.constructDT(
      ls_array = base_array,
      metadata = metadata,
      coeff_extract = tablo_coeff,
      full_exclude = !!full_exclude
    ))
  ))

  # construct tibbles from metadata and dts for each data type
  target_init.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "init.base_tib",
    command = expression(.build_tibble(
      ls_data = ls_dat,
      preagg_header_replace = !!config[["preagg_data"]]
    ))
  ))

  # remove unnecessary headers and map new sets
  target_prepped.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "prepped.base_tib",
    command = expression(.prep_data(
      data_type = "dat",
      tib_data = init.base_tib,
      sets = final.set_tib,
      coeff_list = tablo_coeff
    ))
  ))

  # aggregate to new sets
  target_final.base_tib <- rlang::expr(targets::tar_target_raw(
    name = "final.base_tib",
    command = expression(.aggregate_data(
      data_type = "dat",
      tib_data = prepped.base_tib,
      sets = final.set_tib,
      postagg_header_replace = !!config[["postagg_data"]]
    ))
  ))

  # write data
  target_write.dat <- rlang::expr(targets::tar_target_raw(
    name = "write.dat",
    command = expression(.ragged_write(
      dat = final.base_tib,
      file_name = tab_file_check[["GTAPDATA"]],
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
