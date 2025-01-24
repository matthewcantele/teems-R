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

    cli::cli_abort(c("x" = "The specified internal {file_type} file: {.val {file}} is not supported.",
                     "i" = "Currently supported {file_type} files include: {.val {file_names}}.",
                     "i" = "Path to a user-provided {file_type} file is also supported.",
                     "!" = "Note that user-provided {file_type} files may need to be modified for compatibility with {.fun teems::teems_solver}"))
  } else {
    tab <- valid_internal_files[[file]]
    tab_file <- file.path(tempdir(), paste(file, ext, sep = "."))
    writeLines(text = tab,
               con = tab_file)
  }
  return(tab_file)
}
