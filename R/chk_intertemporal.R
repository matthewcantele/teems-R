#' @importFrom qs2 qs_read
#' @importFrom purrr pluck
#' @importFrom targets tar_read
#' 
#' @keywords internal
#' @noRd
.check_intertemporal <- function(launchpad_dir,
                                 model_dir,
                                 sets) {
  metadata <- qs2::qs_read(file = file.path(launchpad_dir, "metadata.qs2"))
  if (any(sets[["intertemp"]])) {
    intertemporal <- TRUE
    CYRS <- purrr::pluck(.x = targets::tar_read(
      name = final.par_tib,
      store = file.path(model_dir, "store")
    ), "dt", "AYRS")
    CYRS[["Value"]] <- CYRS[["Value"]] + metadata[["reference_year"]]
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
