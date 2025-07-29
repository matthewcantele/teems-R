#' @importFrom targets tar_helper tar_option_set
#' @importFrom purrr pluck discard
#' @importFrom qs2 qs_save
#' 
#' @keywords internal
#' @noRd
.write_pipeline <- function(model_config,
                            load_config,
                            model_name,
                            set_map_files,
                            metadata,
                            teems_paths) {
  model_targets <- .model_control(
    config = model_config,
    metadata = metadata,
    launchpad_dir = teems_paths$launchpad
  )

  set_map_files <- .get_setmap_info(config = load_config$set_mappings)
  set_targets <- .set_control(
    set_input = load_config$set_input,
    set_map_files = set_map_files,
    data_type = "set",
    write_dir = teems_paths$launchpad,
    set_hash = attr(load_specs, "call_id")
  )

  data_targets <- .data_control(
    dat_input = load_config$dat_input,
    par_input = load_config$par_input,
    aux_input = load_config$aux_input,
    unaggregated_input = load_config$unaggregated_input,
    aggregated_input = load_config$aggregated_input,
    write_dir = teems_paths$launchpad
  )

  # we could move the hash table up and remove in parent env
  if (!is.null(model_config$swap_in)) {
    model_config$swap_in <- purrr::map(model_config$swap_in, function(x) {
      attr(x, "call") <- NULL
      return(x)
    })
  }
  
  if (!is.null(model_config$swap_out)) {
    model_config$swap_out <- purrr::map(model_config$swap_out, function(x) {
      attr(x, "call") <- NULL
      return(x)
    })
  }
  
  closure_targets <- .closure_control(
    closure_file = model_config$closure_file,
    swap_in = model_config$swap_in,
    swap_out = model_config$swap_out,
    var_omit = model_config$var_omit,
    write_dir = teems_paths$launchpad,
    model_name = model_name
  )

  if (!is.null(model_config$shock)) {
    model_config$shock <- purrr::map(model_config$shock, function(x) {
      attr(x, "call") <- NULL
      return(x)
    })
  }

  shock_targets <- .shock_control(
    shock = model_config$shock,
    shock_file = model_config$shock_file,
    write_dir = teems_paths$launchpad
  )

  targets <- c(
    model_targets,
    set_targets,
    data_targets,
    closure_targets,
    shock_targets
  )

  targets::tar_helper(teems_paths$pipeline, {
    targets::tar_option_set(
      format = "qs",
      memory = "transient",
      garbage_collection = TRUE,
      envir = getNamespace(name = "teems")
    )
    !!targets
  })

  qs2::qs_save(
    metadata,
    file.path(teems_paths$launchpad, "metadata.qs2")
  )

  return(targets)
}