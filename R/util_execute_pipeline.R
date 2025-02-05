.execute_pipeline <- function(teems_paths,
                              model_name,
                              metadata,
                              quiet,
                              .testing) {
  if (!.testing) {
    targets::tar_make(
      script = teems_paths[["pipeline"]],
      store = teems_paths[["store"]])
  } else {
    targets::tar_make(
      script = teems_paths[["pipeline"]],
      callr_function = NULL,
      store = teems_paths[["store"]])
  }

  gen_out <- .cmf_write(model_name = model_name,
                        model_dir = teems_paths[["model"]],
                        store_dir = teems_paths[["store"]],
                        launchpad_dir = teems_paths[["launchpad"]])
  if (!quiet) {
  # .pipeline_diagnostics(model_dir = teems_paths[["model"]],
  #                       launchpad_dir = teems_paths[["launchpad"]],
  #                       model_name = model_name,
  #                       metadata = metadata,
  #                       store_dir = teems_paths[["store"]],
  #                       io_files = gen_out[["io_files"]])
  }
  
  return(gen_out[["cmf_path"]])
}