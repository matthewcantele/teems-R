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
  numeric_ext <- !is.na(suppressWarnings(as.numeric(file_ext)))
  if (!identical(x = file_ext, y = "") && !numeric_ext) {
    file <- .check_usr_file(
      file = file,
      valid_ext = valid_ext,
      file_ext = file_ext,
      arg = arg,
      call = call
    )
  } else if (internal) {
    file <- .check_internal_file(
      file = file,
      ext = valid_ext,
      call = call
    )
  }
  return(file)
}

.check_usr_file <- function(file,
                            valid_ext,
                            file_ext,
                            arg = arg,
                            call) {
  if (!file.exists(file)) {
    .cli_action(msg = "Cannot open file {.file {file}}: No such file.",
                action = "abort",
                call = call)
  } else if (!is.element(el = file_ext, set = valid_ext)) {
    .cli_action(msg = "{.arg {arg}} must be a {.or {.val {valid_ext}}} file, 
                not {?a/an} {.val {file_ext}} file.",
                action = "abort",
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
    .cli_action(msg = c("The specified internal {file_type} file: {.val {file}} 
                        is not supported.",
                        "Currently supported {file_type} files include: 
                        {.val {file_names}}.",
                        "Alternatively, path to a user-provided {file_type} 
                        file is supported.",
                        "Note that user-provided {file_type} files may need to 
                        be modified for compatibility with various {.pkg teems} 
                        functions."),
                action = c("abort", rep(x = "inform", 3)),
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