#' @importFrom targets tar_helper tar_option_set
#' @importFrom qs2 qs_save
#' 
#' @keywords internal
#' @noRd
.write_pipeline <- function(model_config,
                            data_config,
                            set_config,
                            closure_config,
                            shock_config,
                            set_map_files,
                            n_timestep,
                            metadata,
                            teems_paths) {

  model_targets <- .model_control(config = model_config,
                                  metadata = metadata,
                                  launchpad_dir = teems_paths[["launchpad"]])
   
  set_targets <- .set_control(config = set_config,
                              set_map_files = set_map_files,
                              n_timestep = n_timestep,
                              data_type = "set",
                              full_exclude = model_config[["full_exclude"]],
                              write_dir = teems_paths[["launchpad"]])
   
  data_targets <- .data_control(config = data_config,
                                ndigits = model_config[["ndigits"]],
                                full_exclude = model_config[["full_exclude"]],
                                write_dir = teems_paths[["launchpad"]])
   
  closure_targets <- .closure_control(config = closure_config,
                                      var_omit = model_config[["var_omit"]],
                                      write_dir = teems_paths[["launchpad"]],
                                      model_name = model_name)
   
  shock_targets <- .shock_control(shock = closure_config[["shock"]],
                                  shock_file = closure_config[["shock_file"]],
                                  ndigits = model_config[["ndigits"]],
                                  write_dir = teems_paths[["launchpad"]])
   
  targets <- c(model_targets,
               set_targets,
               data_targets,
               closure_targets,
               shock_targets)
   
  targets::tar_helper(path = teems_paths[["pipeline"]], {
    targets::tar_option_set(
      format = "qs",
      memory = "transient",
      garbage_collection = TRUE,
      envir = getNamespace(name = "teems")
    )
    !!targets
  })

  qs2::qs_save(object = metadata,
               file = file.path(teems_paths[["launchpad"]], "metadata.qs2"))
  
  return(targets)
}