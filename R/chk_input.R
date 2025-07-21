#' @importFrom tools file_ext
#'
#' @keywords internal
#' @noRd
.check_input <- function(file = NULL,
                         valid_ext,
                         call,
                         cache = TRUE,
                         internal = TRUE,
                         hash = NULL) {
  # file to object
  if (!is.list(file)) {
    arg <- as.character(substitute(file))
    file_ext <- tolower(tools::file_ext(basename(file)))
    numeric_ext <- !is.na(suppressWarnings(as.numeric(file_ext)))
    if (dir.exists(file)) {
      .cli_action(
        gen_err$dir_not_file,
        action = "abort",
        call = call
      )
    }
    if (!identical(file_ext, "") && !numeric_ext) {
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
    if (cache) {
      file_name <- paste0(attr(file, "data_type"), hash)
      file <- .teems_cache(
        input = file,
        file = file_name,
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
    .cli_action(gen_err$no_file,
      action = "abort",
      call = call
    )
  } else if (!file_ext %in% valid_ext) {
    .cli_action(gen_err$invalid_file,
      action = "abort",
      call = call
    )
  }

  file_path <- normalizePath(file)
  if (cache) {
    dir <- paste(file_ext, "files", sep = "_")
    file_path <- .teems_cache(
      file = file_path,
      ext = file_ext,
      dir = dir
    )
  }

  attr(file_path, which = "file_ext") <- file_ext
  return(file_path)
}

.check_internal_file <- function(file,
                                 call,
                                 ext) {
  valid_internal_files <- get(paste("internal", ext, sep = "_"))
  file_names <- names(valid_internal_files)
  if (!file %in% file_names) {
    file_type <- switch(ext,
      "tab" = "Tablo",
      "cls" = "closure"
    )
    if (!identical(ext, "cls")) {
      .cli_action(gen_err$invalid_internal,
        action = c("abort", rep("inform", 3)),
        url = gen_url$internal_files,
        hyperlink = NULL,
        call = call
      )
    } else {
      .cli_action(cls_err$invalid_internal,
        action = c("abort", rep("inform", 3)),
        url = cls_url$internal_files,
        hyperlink = NULL,
        call = call
      )
    }
  } else {
    input <- valid_internal_files[[file]]
    dir <- paste(ext, "files", sep = "_")
    file_path <- .teems_cache(
      input = input,
      file = file,
      ext = ext,
      dir = dir
    )
  }
  return(file_path)
}