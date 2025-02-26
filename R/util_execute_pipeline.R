#' @importFrom targets tar_make tar_load_everything tar_visnetwork
#' 
#' @keywords internal
#' @noRd
.execute_pipeline <- function(teems_paths,
                              model_name,
                              metadata,
                              quiet,
                              tar_load_everything,
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

  if (tar_load_everything) {
    targets::tar_load_everything(store = teems_paths[["store"]],
                                 envir = .GlobalEnv)
  }
  
  gen_out <- .write_cmf(model_name = model_name,
                        model_dir = teems_paths[["model"]],
                        store_dir = teems_paths[["store"]],
                        launchpad_dir = teems_paths[["launchpad"]])
  .pipeline_diagnostics(model_dir = teems_paths[["model"]],
                        launchpad_dir = teems_paths[["launchpad"]],
                        model_name = model_name,
                        metadata = metadata,
                        store_dir = teems_paths[["store"]],
                        io_files = gen_out[["io_files"]],
                        quiet = quiet)

  return(gen_out[["cmf_path"]])
}