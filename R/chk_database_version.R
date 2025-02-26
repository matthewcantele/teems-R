#' @importFrom cli cli_warn
#'
#' @keywords internal
#' @noRd
.check_database_version <- function(vetted,
                                    provided,
                                    quiet) {
  if (!is.element(el = provided, set = vetted)) {
    teems_version <- packageVersion("teems")
    cli::cli_warn(c("{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}",
      "!" = "The database provided has a highly probability of triggering an error within the {.pkg targets} pipeline.",
      "i" = "The {.fn teems::teems_solve} function can bypass the pipeline and be called on solver-ready input files."
    ))
  }
}
