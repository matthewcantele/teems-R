#' @keywords internal
#' @noRd
.get_postmodel_paths <- function(cmf_path,
                                 call) {
  launchpad_dir <- dirname(path = normalizePath(path = cmf_path))
  model_dir <- sub(pattern = "/launchpad",
                   replacement = "",
                   x = launchpad_dir)
  tab_path <- grep(pattern = "modified_",
                   x = list.files(path = launchpad_dir, full.names = TRUE),
                   value = TRUE)
  if (!dir.exists(paths = model_dir)) {
    .cli_action(action = "abort",
                msg = "Implied model directory {.path {model_dir}} not found. This 
              would indicate that the {.arg cmf_path} provided, {.path 
              {cmf_path}}, is not correct.",
                call = call)
  }
  metadata_path <- file.path(launchpad_dir, "metadata.qs2")
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
  if (identical(x = character(0), y = var_paths)) {
    .cli_action(action = "abort",
                msg = "Model outputs not found. This would indicate that a 
                model run at the {.arg cmf_path} provided, {.path {cmf_path}}, 
                has not taken place.",
                call = call)
  }
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
  paths <- list(tab = tab_path,
                launchpad = launchpad_dir,
                model = model_dir,
                metadata = metadata_path,
                var = var_paths,
                coeff = coeff_paths,
                set = set_paths)
  return(paths)
}