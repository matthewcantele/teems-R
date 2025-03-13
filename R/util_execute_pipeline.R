#' @importFrom targets tar_make tar_load_everything tar_visnetwork
#' 
#' @keywords internal
#' @noRd
.execute_pipeline <- function(teems_paths,
                              model_name,
                              metadata,
                              base_call,
                              par_call,
                              set_call,
                              quiet,
                              tar_load_everything,
                              .testing) {
  if (!.testing) {
    output <- try({
      targets::tar_make(
        script = teems_paths[["pipeline"]],
        store = teems_paths[["store"]])
    }, silent = TRUE)
    if (inherits(x = output, what = "try-error")) {
      errored_tar <- targets::tar_errored(store = teems_paths[["store"]])
      raw_error <- targets::tar_meta(names = errored_tar,
                                     store = teems_paths[["store"]])[["error"]]
      error_inputs <- trimws(x = strsplit(x = raw_error,
                                          split =  "(?<!:):(?!:)",
                                          perl = TRUE)[[1]][2])
      expr <- parse(text = error_inputs)
      eval(expr = expr)
    }
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