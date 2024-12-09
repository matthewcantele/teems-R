#' @keywords internal
#' @noRd
# Prepare an internal function to handle message printing or file output
.diagnostic_message <- function(message_text,
                           verbose,
                           write_path) {

  if (verbose) {
    message(message_text)
  }

  # write to diagnostic file
  capture.output(cat(message_text, "\n"),
                 file = write_path,
                 append = TRUE)

}
