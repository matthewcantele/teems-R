.write_pipeline <- function(model_config,
                            set_config,
                            param_config,
                            base_config,
                            closure_config,
                            shock_config,
                            metadata,
                            teems_paths) {

  model_targets <- .model_config(config = model_config,
                                 launchpad_dir = teems_paths[["launchpad"]])
  
  set_targets <- .set_config(config = set_config,
                             metadata = metadata,
                             full_exclude = model_config[["full_exclude"]],
                             write_dir = teems_paths[["launchpad"]])
  
  param_targets <- .param_config(config = param_config,
                                 metadata = metadata,
                                 ndigits = model_config[["ndigits"]],
                                 full_exclude = model_config[["full_exclude"]],
                                 write_dir = teems_paths[["launchpad"]])
  
  base_targets <- .base_config(config = base_config,
                               metadata = metadata,
                               ndigits = model_config[["ndigits"]],
                               full_exclude = model_config[["full_exclude"]],
                               write_dir = teems_paths[["launchpad"]])
  
  closure_targets <- .closure_config(config = closure_config,
                                     tab_file = model_config[["tab_file"]],
                                     write_dir = teems_paths[["launchpad"]],
                                     model_name = model_name)
  
  shock_targets <- .shock_config(shock = closure_config[["shock"]],
                                 shock_file = closure_config[["shock_file"]],
                                 ndigits = model_config[["ndigits"]],
                                 write_dir = teems_paths[["launchpad"]])
  
  targets <- c(model_targets,
               set_targets,
               param_targets,
               base_targets,
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
  
  return(targets)
}