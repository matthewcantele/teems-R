#' @importFrom cli cli_abort
#' @importFrom tools file_ext
#'
#' @keywords internal
#' @noRd
.check_input <- function(file,
                         valid_ext,
                         call,
                         internal = TRUE) {
  arg <- as.character(x = substitute(expr = file))
  file_ext <- tolower(x = tools::file_ext(x = basename(path = file)))
  if (!identical(x = file_ext, y = "")) {
    file <- .check_usr_file(file = file,
                            valid_ext = valid_ext,
                            file_ext = file_ext,
                            arg = arg,
                            call = call)
  } else if (internal) {
    file <- .check_internal_file(file = file,
                                 ext = valid_ext,
                                 call = call)
  }
  return(file)
}

.check_usr_file <- function(file,
                            valid_ext,
                            file_ext,
                            arg = arg,
                            call) {
  if (!file.exists(file)) {
    cli::cli_abort(c("x" = "Cannot open file {.file {file}}: No such file."),
                   call = call)
  } else if (!is.element(el = file_ext, set = valid_ext)) {
      cli::cli_abort(c("x" = "{.arg {arg}} must be a {.or {valid_ext}} file, not {?a/an} {file_ext} file."),
                     call = call)
  }
  file <- path.expand(path = file)
  attr(x = file, which = "file_ext") <- file_ext
  return(file)
}

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
    input <- valid_internal_files[[file]]
    dir <- paste(ext, "files", sep = "_")
    file_path <- .teems_cache(input = input,
                              file = file,
                              ext = ext,
                              dir = dir)
  }
  return(file_path)
}