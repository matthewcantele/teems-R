#' @keywords internal 
#' @noRd
.path_ledger <- function(base_dir,
                         model_name,
                         call) {

  if (!identical(x = base_dir, y = tools::R_user_dir("teems", "data"))) {
    base_dir <- normalizePath(path = base_dir)
    if (!dir.exists(path = base_dir)) {
      .cli_action(action = "abort",
                  msg = "The path provided for {.arg base_dir}, {.path {base_dir}}, does not exist.",
                  call = call)
    }
  }
  
  if (!dir.exists(base_dir)) {
    dir.create(base_dir,
               recursive = TRUE)
  }
  
  model_dir <- file.path(base_dir, model_name)
  pipeline_file <- file.path(model_dir, paste0(model_name, ".R"))
  store_dir <- file.path(model_dir, "store")
  launchpad_dir <- file.path(model_dir, "launchpad")
  if (!dir.exists(paths = model_dir)) {
    dir.create(path = model_dir, recursive = TRUE)
  }
  if (!dir.exists(paths = store_dir)) {
    dir.create(path = store_dir, recursive = TRUE)
  }
  if (dir.exists(paths = launchpad_dir)) {
    unlink(x = launchpad_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(path = file.path(launchpad_dir, "out", "sets"),
             recursive = TRUE)
  dir.create(path = file.path(launchpad_dir, "out", "coefficients"),
             recursive = TRUE)
  dir.create(path = file.path(launchpad_dir, "out", "variables", "bin"),
             recursive = TRUE)
  paths <- list(base = base_dir,
                model = model_dir,
                pipeline = pipeline_file,
                store = store_dir,
                launchpad = launchpad_dir)
  return(paths)
}