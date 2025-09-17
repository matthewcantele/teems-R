#' @importFrom tools file_ext
#'
#' @keywords internal
#' @noRd
.check_input <- function(file = NULL,
                         valid_ext,
                         call) {

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
        call = call
      )
    } else {
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
  # class here
  return(file_path)
}

.check_internal_file <- function(file,
                                 call,
                                 ext) {

  valid_internal_files <- names(get(paste("internal", ext, sep = "_")))
  if (!file %in% valid_internal_files) {
    file_type <- switch(ext,
      "tab" = "Tablo",
      "cls" = "closure"
    )
    if (!identical(ext, "cls")) {
      .cli_action(gen_err$invalid_internal,
        action = c("abort", rep("inform", 3)),
        hyperlink = NULL,
        call = call
      )
    } else {
      .cli_action(cls_err$invalid_internal,
        action = c("abort", rep("inform", 3)),
        hyperlink = NULL,
        call = call
      )
    }
  }

  class(file) <- "internal"
  return(file)
}
