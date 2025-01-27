#' @importFrom cli cli_abort
#'
#' @keywords internal
#' @noRd
.check_internal_file <- function(file,
                                 call,
                                 ext) {
  valid_internal_files <- get(x = paste("internal", ext, sep = "_"))
  file_names <- names(x = valid_internal_files)
  if (!is.element(el = file, set = file_names)) {
    file_type <- switch(
      EXPR = ext,
      "tab" = "Tablo",
      "cls" = "closure"
    )

    .dev_trace()
    cli::cli_abort(c("x" = "The specified internal {file_type} file: {.val {file}} is not supported.",
                     "i" = "Currently supported {file_type} files include: {.val {file_names}}.",
                     "i" = "Alternatively, path to a user-provided {file_type} file is supported.",
                     "!" = "Note that user-provided {file_type} files may need to be modified for compatibility with various {.pkg teems} functions."),
                   call = call)
  } else {
    tab <- valid_internal_files[[file]]
    tab_file <- .teems_cache(input = tab,
                             file = file,
                             ext = ext,
                             dir = "tab_files")
  }
  return(tab_file)
}
