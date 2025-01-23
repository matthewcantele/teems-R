#' @importFrom cli cli_abort cli_inform
#'
#' @keywords internal
#' @noRd
.get_model_version <- function(tab_file,
                               quiet,
                               call) {
  tab <- readChar(con = tab_file,
                  nchars = file.info(tab_file)[["size"]])
  # get tab data format (first 300 char)
  n_char <- 300
  tab_preface <- substring(text = tab, first = 1, last = n_char)
  lines <- unlist(x = strsplit(x = tab_preface, split = "\n"))
  if (any(grepl(pattern = "6.2", x = lines))) {
    model_version <- "v6.2"
  } else if (any(grepl(pattern = "7.0|7", x = lines))) {
    model_version <- "v7.0"
  } else {
    cli::cli_abort(message = c("Model version was not successfully extracted from the Tablo file.",
                               "Try inputing {.arg model_version} explicitly or modifying the Tablo file.",
                               "i" = "If not explicitly provided, {.pkg teems} currently calls {.fn grepl} on the first {n_char} lines using patterns {.val 6.2} and {.val 7.0|7}."),
                   call = call)
  }
  if (!quiet) {
  cli::cli_inform(message = "Model version for the provided Tablo file has been determined as {.val {model_version}}.")
  }
  return(model_version)
}
