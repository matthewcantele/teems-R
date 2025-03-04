#' @importFrom rlang current_env
#'
#' @keywords internal
#' @noRd
.inform_metadata <- function(metadata,
                             quiet) {
  if (!quiet) {
    list2env(x = metadata,
             envir = rlang::current_env())
    .cli_action("Reference year: {.val {reference_year}}",
                "Data format: {.val {data_format}}",
                action = "inform",
                msg = "GTAP Data Base version: {.val {full_database_version}}")
  }
}
