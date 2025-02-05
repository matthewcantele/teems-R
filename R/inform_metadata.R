#' @importFrom cli cli_inform
#'
#' @keywords internal
#' @noRd
.inform_metadata <- function(metadata,
                             quiet) {
  if (!quiet) {
    with(data = metadata,
         expr = cli::cli_inform(c(
           "i" = "GTAP Data Base version: {.val {full_database_version}}",
           "i" = "Reference year: {.val {reference_year}}",
           "i" = "Data format: {.val {data_format}}"
         )))
  }
}
