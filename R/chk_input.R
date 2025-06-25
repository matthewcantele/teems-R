#' @importFrom tools file_ext
#'
#' @keywords internal
#' @noRd
.check_input <- function(file = NULL,
                         valid_ext,
                         call,
                         cache = TRUE,
                         internal = TRUE) {
  # file to object
  if (!is.list(x = file)) {
    arg <- as.character(x = substitute(expr = file))
    file_ext <- tolower(x = tools::file_ext(x = basename(path = file)))
    numeric_ext <- !is.na(suppressWarnings(as.numeric(file_ext)))
    if (dir.exists(paths = file)) {
      .cli_action(
        msg = "A filepath is expected, not the directory {.file {file}}.",
        action = "abort",
        call = call
      )
    }
    if (!identical(x = file_ext, y = "") && !numeric_ext) {
      file <- .check_usr_file(
        file = file,
        valid_ext = valid_ext,
        file_ext = file_ext,
        arg = arg,
        cache = cache,
        call = call
      )
    } else if (internal) {
      file <- .check_internal_file(
        file = file,
        ext = valid_ext,
        call = call
      )
    }
  } else {
    # fix this
    if (cache) {
      file <- .teems_cache(
        input = file,
        file = deparse(substitute(file)),
        ext = "qs2",
        dir = "inputdata"
      )
    }
  }
  return(file)
}
.check_usr_file <- function(file,
                            valid_ext,
                            file_ext,
                            arg = arg,
                            cache = cache,
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

  file <- normalizePath(path = file)
  dir <- paste(file_ext, "files", sep = "_")
  file_path <- .teems_cache(file = file,
                            ext = file_ext,
                            dir = dir)
  attr(x = file_path, which = "file_ext") <- file_ext
  return(file_path)
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
    if (!identical(x = ext, y = "cls")) {
    .cli_action(msg = c("The specified internal {file_type} file: {.val {file}} 
                        is not supported.",
                        "Currently supported {file_type} files include: 
                        {.val {file_names}}.",
                        "Alternatively, path to a user-provided {file_type} 
                        file is supported (e.g., \"/my/{file_type}/path.{ext}\")",
                        "Note that user-provided {file_type} files may need to 
                        be modified for compatibility with various {.pkg teems} 
                        functions (link here)."),
                action = c("abort", rep(x = "inform", 3)),
                call = call)
    } else {
      .cli_action(msg = c("The closure file inferred from the provided 
      {.arg tab_file}: {.val {file}} does not exist.",
                          "Currently supported internal {file_type} files are available for: 
                        {.val {file_names}}.",
                          "Alternatively, path to a user-provided {file_type} 
                        file is supported (e.g., \"/my/{file_type}/path.{ext}\")",
                          "Note that user-provided {file_type} files may need to 
                        be modified for compatibility with various {.pkg teems} 
                        functions (link here)."),
                  action = c("abort", rep(x = "inform", 3)),
                  call = call)
    }
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