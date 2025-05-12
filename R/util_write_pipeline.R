#' @importFrom targets tar_helper tar_option_set
#' @importFrom qs2 qs_save
#' 
#' @keywords internal
#' @noRd
.write_pipeline <- function(model_config,
                            set_config,
                            param_config,
                            base_config,
                            closure_config,
                            shock_config,
                            metadata,
                            teems_paths) {

  model_targets <- .model_control(config = model_config,
                                  launchpad_dir = teems_paths[["launchpad"]])

  set_targets <- .set_control(config = set_config,
                              set_extract = model_config[["set_extract"]],
                              coeff_extract = model_config[["coeff_extract"]],
                              data_type = "set",
                              metadata = metadata,
                              full_exclude = model_config[["full_exclude"]],
                              write_dir = teems_paths[["launchpad"]])
  
  param_targets <- .param_control(config = param_config,
                                  coeff_extract = model_config[["coeff_extract"]],
                                  set_extract = model_config[["set_extract"]],
                                  data_type = "par",
                                  metadata = metadata,
                                  ndigits = model_config[["ndigits"]],
                                  full_exclude = model_config[["full_exclude"]],
                                  write_dir = teems_paths[["launchpad"]])
  
  base_targets <- .base_control(config = base_config,
                                coeff_extract = model_config[["coeff_extract"]],
                                set_extract = model_config[["set_extract"]],
                                data_type = "base",
                                metadata = metadata,
                                ndigits = model_config[["ndigits"]],
                                full_exclude = model_config[["full_exclude"]],
                                write_dir = teems_paths[["launchpad"]])
  
  closure_targets <- .closure_control(config = closure_config,
                                      var_extract = model_config[["var_extract"]],
                                      var_omit = model_config[["var_omit"]],
                                      write_dir = teems_paths[["launchpad"]],
                                      model_name = model_name)
  
  shock_targets <- .shock_control(shock = closure_config[["shock"]],
                                  shock_file = closure_config[["shock_file"]],
                                  var_extract = model_config[["var_extract"]],
                                  metadata = metadata,
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
  
  qs2::qs_save(object = metadata,
               file = file.path(teems_paths[["launchpad"]], "metadata.qs2"))
  
  return(targets)
}