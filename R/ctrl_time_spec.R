#' Time specifications
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
.time_config <- function(config,
                         temporal_dynamics,
                         ndigits,
                         write_dir) {
  if (identical(x = temporal_dynamics, y = "intertemporal")) {
    # conditional file tracking
    if (grepl(pattern = "\\.csv", x = config[["CPHI"]])) {
      target_CPHI_file <- rlang::expr(targets::tar_target_raw(
        name = "CPHI_file",
        command = quote(expr = !!config[["CPHI"]]),
        format = "file"
      ))

      target_CPHI <- rlang::expr(targets::tar_target_raw(
        name = "CPHI",
        command = expression({
          read.csv(file = CPHI_file)
        })
      ))
    } else {
      target_CPHI <- rlang::expr(targets::tar_target_raw(
        name = "CPHI",
        command = quote(expr = !!config[["CPHI"]])
      ))
    }

    if (grepl(pattern = "\\.csv", x = config[["LRORG"]])) {
      target_LRORG_file <- rlang::expr(targets::tar_target_raw(
        name = "LRORG_file",
        command = quote(expr = !!config[["LRORG"]]),
        format = "file"
      ))

      target_LRORG <- rlang::expr(targets::tar_target_raw(
        name = "LRORG",
        command = expression({
          read.csv(file = LRORG_file)
        })
      ))
    } else {
      target_LRORG <- rlang::expr(targets::tar_target_raw(
        name = "LRORG",
        command = quote(expr = !!config[["LRORG"]])
      ))
    }

    if (grepl(pattern = "\\.csv", x = config[["KAPPA"]])) {
      target_KAPPA_file <- rlang::expr(targets::tar_target_raw(
        name = "KAPPA_file",
        command = quote(expr = !!config[["KAPPA"]]),
        format = "file"
      ))

      target_KAPPA <- rlang::expr(targets::tar_target_raw(
        name = "KAPPA",
        command = expression({
          read.csv(file = KAPPA_file)
        })
      ))
    } else {
      target_KAPPA <- rlang::expr(targets::tar_target_raw(
        name = "KAPPA",
        command = quote(expr = !!config[["KAPPA"]])
      ))
    }

    # generate time sets (for pre-post model set checks)
    target_time_sets <- rlang::expr(targets::tar_target_raw(
      name = "time_sets",
      command = expression(.time_sets(
        time_steps = !!config[["time_steps"]]
      )),
      cue = targets::tar_cue(mode = "always")
    ))

    # intertemporal parameters
    target_int_param <- rlang::expr(targets::tar_target_raw(
      name = "int_param",
      command = expression(.param_int(
        INID = !!config[["INIDELTA"]],
        CPHI = CPHI,
        IRAT = LRORG,
        KAPP = KAPPA,
        sets = final.set_tib
      )),
      cue = targets::tar_cue(mode = "always")
    ))

    # generate time coefficients
    target_time_coeff <- rlang::expr(targets::tar_target_raw(
      name = "time_coeff",
      command = expression(.time_coeff(
        sets = final.set_tib,
        time_steps = !!config[["time_steps"]],
        base_year = metadata[["reference_year"]]
      ))
    ))

    # Write intertemporal parameters
    target_write.int_par <- rlang::expr(targets::tar_target_raw(
      name = "write.int_par",
      command = expression(.ragged_write(
        dat = int_param,
        file_name = tab_file_check[["INTPARM"]],
        out_dir = !!write_dir,
        ndigits = !!ndigits
      )),
      format = "file",
      cue = targets::tar_cue(mode = "always")
    ))

    # Write intertemporal data
    target_write.int_dat <- rlang::expr(targets::tar_target_raw(
      name = "write.int_dat",
      command = expression(.ragged_write(
        dat = time_coeff,
        file_name = tab_file_check[["INTDATA"]],
        out_dir = !!write_dir,
        ndigits = 0
      )),
      format = "file",
      cue = targets::tar_cue(mode = "always")
    ))
  } else {
    target_time_sets <- rlang::expr(targets::tar_target_raw(
      name = "time_sets",
      command = quote(NULL)
    ))

    target_time_coeff <- rlang::expr(targets::tar_target_raw(
      name = "time_coeff",
      command = quote(NULL)
    ))

    target_write.int_dat <- rlang::expr(targets::tar_target_raw(
      name = "write.int_dat",
      command = quote(NULL),
      cue = targets::tar_cue(mode = "always")
    ))

    target_int_param <- rlang::expr(targets::tar_target_raw(
      name = "int_param",
      command = quote(NULL)
    ))

    target_write.int_par <- rlang::expr(targets::tar_target_raw(
      name = "write.int_par",
      command = quote(NULL),
      cue = targets::tar_cue(mode = "always")
    ))
  }
  ##############################################################################
  # gather and check all generated targets
  targets <- .gather_targets(criteria = "target_")
  return(targets)
}
