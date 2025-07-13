#' @importFrom rlang expr current_env
#' @importFrom qs2 qd_read
#' @importFrom targets tar_target_raw tar_cue
#' @importFrom purrr pluck
#'
#' @keywords internal
#' @noRd
.data_control <- function(config,
                          full_exclude,
                          write_dir) {
  t_data_call <- rlang::expr(targets::tar_target_raw(
    "data_call",
    rlang::expr(quote(!!config$call))
  ))

  t_dat_file <- rlang::expr(targets::tar_target_raw(
    "dat_file",
    quote(!!config$dat_input),
    format = "file"
  ))

  dat_file_type <- attr(config$dat_input, "file_ext")

  if (dat_file_type %=% "har") {
    t_dat_array <- rlang::expr(targets::tar_target_raw(
      "dat_array",
      expression(.read_har(
        con = dat_file,
        data_type = "dat",
        call = data_call
      ))
    ))
  } else if (dat_file_type %=% "qs2") {
    t_dat_array <- rlang::expr(targets::tar_target_raw(
      "dat_array",
      expression(qs2::qs_read(dat_file))
    ))
  }

  t_par_file <- rlang::expr(targets::tar_target_raw(
    "par_file",
    quote(!!config$par_input),
    format = "file"
  ))

  par_file_type <- attr(x = config$par_input, which = "file_ext")

  if (par_file_type %=% "har") {
    t_par_array <- rlang::expr(targets::tar_target_raw(
      "par_array",
      expression(.read_har(
        con = par_file,
        data_type = "par",
        call = data_call
      ))
    ))
  } else if (par_file_type %=% "qs2") {
    t_par_array <- rlang::expr(targets::tar_target_raw(
      "par_array",
      expression(qs2::qs_read(par_file))
    ))
  }

  if (!is.null(config$aux_input)) {
    aux_file_type <- attr(config$aux_input, "file_ext")
    t_aux_file <- rlang::expr(targets::tar_target_raw(
      "aux_file",
      quote(!!config$aux_input),
      format = "file"
    ))
    if (aux_file_type %=% "har") {
      t_aux_array <- rlang::expr(targets::tar_target_raw(
        "aux_array",
        expression(.read_har(
          con = aux_file,
          call = data_call
        ))
      ))
    } else if (aux_file_type %=% "qs2") {
      t_aux_array <- rlang::expr(targets::tar_target_raw(
        "aux_array",
        expression(qs2::qs_read(aux_file))
      ))
    }
  } else {
    t_aux_array <- rlang::expr(targets::tar_target_raw(
      "aux_array",
      quote(NULL)
    ))
  }

  t_data_mod <- rlang::expr(targets::tar_target_raw(
    "mod.data_array",
    expression(.modify_array(
      dat_array = dat_array,
      par_array = par_array,
      aux_array = aux_array,
      set_array = mod.set_array,
      metadata = metadata,
      full_exclude = !!full_exclude,
      coeff_extract = tab_comp$coeff_extract,
      call = data_call
    ))
  ))

  t_data_setnames <- rlang::expr(targets::tar_target_raw(
    "final.data_array",
    expression(.update_set_names(
      ls_array = mod.data_array,
      coeff_extract = tab_comp$coeff_extract,
      data_format = metadata$data_format,
      set_extract = purrr::pluck(tab_comp, "set_extract", "sets")
    ))
  ))

  t_init.data_tib <- rlang::expr(targets::tar_target_raw(
    "init.data_tib",
    expression(.build_tibble(
      ls_array = final.data_array,
      set_extract = purrr::pluck(tab_comp, "set_extract", "sets"),
      int_sets = int_sets,
      unaggregated_input = !!config$unaggregated_input,
      coeff_extract = tab_comp$coeff_extract
    ))
  ))

  t_weighted.data_tib <- rlang::expr(targets::tar_target_raw(
    "weighted.data_tib",
    expression(
      .weight_param(
        tib_data = init.data_tib,
        data_format = metadata$data_format
      )
    )
  ))

  t_prepped.data_tib <- rlang::expr(targets::tar_target_raw(
    "prepped.data_tib",
    expression(.prep_data(
      tib_data = weighted.data_tib,
      sets = final.set_tib,
      coeff_extract = tab_comp$coeff_extract
    ))
  ))

  t_aggregated.data_tib <- rlang::expr(targets::tar_target_raw(
    "aggregated.data_tib",
    expression(.aggregate_data(
      tib_data = prepped.data_tib,
      sets = final.set_tib
    ))
  ))

  t_final.data_tib <- rlang::expr(targets::tar_target_raw(
    "final.data_tib",
    expression(.finalize_data(
      tib_data = aggregated.data_tib,
      aggregated_input = !!config$aggregated_input,
      sets = final.set_tib
    ))
  ))

  t_write.data <- rlang::expr(targets::tar_target_raw(
    "write.data",
    expression(.ragged_write(
      dat = final.data_tib,
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