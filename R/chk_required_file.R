#' @importFrom cli cli_abort
#' @importFrom tools file_ext
#'
#' @keywords internal
#' @noRd
.check_required_file <- function(file,
                                 ext,
                                 call) {
  file <- path.expand(path = file)
  if (missing(x = file)) {
    cli::cli_abort(message = "{.arg {rlang::caller_arg(file)}} has not been provided.",
                   call = call)
  } else if (!file.exists(file)) {
    cli::cli_abort(message = "Cannot open file {.file {file}}: No such file.",
                   call = call)
  } else {
    file_ext <- tools::file_ext(x = basename(path = file))
    if (!identical(x = tolower(x = file_ext), y = ext)) {
      cli::cli_abort(message = "{.arg {rlang::caller_arg(file)}} must be a {ext} file, not {file_ext} file.",
                     call = call)
    }
  }
  return(file)
}
