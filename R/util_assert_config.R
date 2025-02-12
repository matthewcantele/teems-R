#' @importFrom cli cli_inform cli_abort
#' @importFrom targets tar_config_set
#' @importFrom rlang call_args
#' 
#' @keywords internal
#' @noRd
.assert_config <- function(model_config,
                           set_config,
                           closure_config,
                           call,
                           store_dir,
                           base_dir,
                           model_name,
                           quiet) {
  if (model_config[["intertemporal"]]) {
    #model_config[["full_exclude"]] <- c(model_config[["full_exclude"]], "RDLT", "RFLX")
    if (is.null(x = set_config[["time_steps"]])) {
      cli::cli_abort(c("x" = "{.arg time_steps} is a required argument for 
                       intertemporal models."),
                     call = call)
    }
  }
  targets::tar_config_set(
    store = store_dir,
    config = file.path(base_dir, "_targets.yaml"),
    project = model_name)

  if (is.null(x = closure_config) || is.null(x = closure_config[["closure_file"]])) {
    if (is.null(x = closure_config)) {
    if (!quiet) {
      cli::cli_inform(
        c("i" = "{.arg closure_config} is not supplied so the
                      standard closure will be used with a null shock.",
          "i" = "A null shock will return all model coefficients as
                      they are provided and/or calculated in the Tablo file.",
          "!" = "Any significantion deviation under these
                      conditions would indicate an error in the loading of
                      input files or parsing of model outputs.")
        # point to teems_parse options
      )
    }
    } else {
      if (!quiet) {
        cli::cli_inform(
          c("i" = "{.arg closure_config} is not supplied so the
                      standard closure will be used.")
          # point to teems_parse options
        )
      }
    }
    tab_file <- rlang::call_args(model_config[["call"]])[["tab_file"]]
    closure <- .infer_closure(tab_file = tab_file)
    closure_file <- .teems_cache(input = closure,
                                 file = tab_file,
                                 ext = "cls",
                                 dir = "cls_files")
    closure_config[["closure_file"]] <- closure_file
  }

return(closure_config)
}