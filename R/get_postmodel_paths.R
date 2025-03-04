#' @keywords internal
#' @noRd
.get_postmodel_paths <- function(cmf_path) {
  launchpad_dir <- dirname(path = path.expand(path = cmf_path))
  model_dir <- sub(pattern = "/launchpad",
                   replacement = "",
                   x = launchpad_dir)
  if (!dir.exists(paths = model_dir)) {
    .cli_action(action = "abort",
                msg = "Implied model directory {.path {model_dir}} not found. This 
              would indicate that the {.arg cmf_path} provided, {.path 
              {cmf_path}}, is not correct or a model run at this location has 
              not taken place.",
                call = call)
  }
  
  var_paths <- list.files(
    path = file.path(
      launchpad_dir,
      "out",
      "variables",
      "bin"
    ),
    pattern = "csvs",
    full.names = TRUE
  )
  coeff_paths <- list.files(
    path = file.path(
      launchpad_dir,
      "out",
      "coefficients"
    ),
    pattern = "csv",
    full.names = TRUE
  )
  set_paths <- list.files(
    path = file.path(
      launchpad_dir,
      "out",
      "sets"
    ),
    pattern = "csv",
    full.names = TRUE
  )

  paths <- list(launchpad = launchpad_dir,
                model = model_dir,
                var = var_paths,
                coeff = coeff_paths,
                set = set_paths)
  return(paths)
}