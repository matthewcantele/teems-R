#' @importFrom rlang current_env
#'
#' @keywords internal
#' @noRd
.inform_metadata <- function(metadata,
                             quiet) {
  if (!quiet) {
    list2env(x = metadata,
             envir = rlang::current_env())
    .cli_action(msg = c("GTAP Data Base version: {.val {full_database_version}}",
                        "Reference year: {.val {reference_year}}",
                        "Data format: {.val {data_format}}"),
                action = rep(x = "inform", 3),
                )
  }
}
