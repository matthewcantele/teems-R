#' @importFrom tools R_user_dir
#' @importFrom data.table fwrite
#'
#' @keywords internal
#' @noRd
.teems_cache <- function(input = NULL,
                         file,
                         ext,
                         dir) {
  teems_cache <- tools::R_user_dir(package = "teems", which = "cache")
  if (missing(dir)) {
    dir <- paste(ext, "files", collapse = "_")
  }
  object_cache <- file.path(teems_cache, dir)
  if (!dir.exists(paths = object_cache)) {
    dir.create(path = object_cache, recursive = TRUE)
  }

  if (!is.null(x = input)) {
    file_path <- file.path(object_cache, paste(file, ext, sep = "."))
    if (is.element(el = ext, set = c("tab", "cls"))) {
      writeLines(
        text = input,
        con = file_path
      )
    } else if (identical(x = ext, y = "csv")) {
      data.table::fwrite(input,
        file = file_path
      )
    } else if (identical(x = ext, y = "qs2")) {
      qs2::qs_save(
        object = input,
        file = file_path
      )
      attr(x = file_path, which = "file_ext") <- "qs2"
    }
  } else {
    file_path <- file.path(object_cache, basename(path = file))
    file.copy(from = file, to = file_path, overwrite = TRUE)
  }

  return(file_path)
}
