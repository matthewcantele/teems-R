#' @keywords internal
#' @noRd
.check_database_version <- function(vetted,
                                    provided,
                                    call,
                                    quiet) {
  if (!quiet) {
    if (!is.element(el = provided, set = vetted)) {
      teems_version <- packageVersion("teems")
      .cli_action("The {.fn teems::teems_solve} function can bypass the pipeline and be called on solver-ready input files.",
        action = "warn",
        msg = "{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}",
        call = call
      )
    }
  }
}
