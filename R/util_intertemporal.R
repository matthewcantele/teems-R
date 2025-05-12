#' @importFrom qs2 qs_read
#' @importFrom purrr pluck
#' @importFrom targets tar_read
#' 
#' @keywords internal
#' @noRd
.is_intertemporal <- function(launchpad_dir,
                              model_dir,
                              metadata_path,
                              sets) {
  metadata <- qs2::qs_read(file = metadata_path)
  if (any(sets[["intertemp"]])) {
    intertemporal <- TRUE
    # eventually remove all targets retrieval commands
    AYRS <- purrr::pluck(.x = targets::tar_read(
      name = final.par_tib,
      store = file.path(model_dir, "store")
    ), "dt", "AYRS")
    AYRS[["Value"]] <- AYRS[["Value"]] + metadata[["reference_year"]]
    CYRS <- AYRS
  } else {
    intertemporal <- FALSE
    CYRS <- NULL
  }
  int <- list(
    intertemporal = intertemporal,
    CYRS = CYRS
  )
  return(int)
}
