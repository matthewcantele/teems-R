#' @importFrom cli cli_inform
#'
#' @keywords internal
#' @noRd
.inform_metadata <- function(metadata,
                             quiet) {
  if (!quiet) {
    with(data = metadata,
         expr = cli::cli_inform(c(
           "i" = "GTAP Data Base version: {.val {orig.database.version}}",
           "i" = "Reference year: {.val {reference.year}}",
           "i" = "Data format: {.val {data.format}}"
         )))
  }
}
