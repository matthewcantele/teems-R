#' @importFrom cli cli_inform
#'
#' @keywords internal
#' @noRd
# Prepare an internal function to handle message printing or file output
.diagnostic_message <- function(message_text,
                           quiet,
                           write_path) {

  if (!quiet) {
    cli::cli_inform(c("i" = message_text))
  }

  # write to diagnostic file
  capture.output(cat(message_text, "\n"),
                 file = write_path,
                 append = TRUE)

}
