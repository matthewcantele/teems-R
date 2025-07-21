#' @importFrom tools R_user_dir
#' @importFrom data.table fwrite
#'
#' @keywords internal
#' @noRd
.teems_cache <- function(input = NULL,
                         file,
                         ext,
                         dir) {
  teems_cache <- tools::R_user_dir("teems", "cache")
  if (missing(dir)) {
    dir <- paste(ext, "files", collapse = "_")
  }
  object_cache <- file.path(teems_cache, dir)
  if (!dir.exists(object_cache)) {
    dir.create(object_cache, recursive = TRUE)
  }

  if (!is.null(input)) {
    file_path <- file.path(object_cache, paste(file, ext, sep = "."))
    if (ext %in% c("tab", "cls")) {
      writeLines(input, file_path)
    } else if (ext %=% "csv") {
      data.table::fwrite(input,
        file = file_path
      )
    } else if (ext %=% "qs2") {
      if (!file.exists(file_path)) {
        qs2::qs_save(input, file_path)
      }
      attr(file_path, "file_ext") <- "qs2"
    }
  } else {
    file_path <- file.path(object_cache, basename(file))
    file.copy(file, file_path, overwrite = TRUE)
  }

  return(file_path)
}