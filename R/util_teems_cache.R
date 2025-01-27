#' @importFrom tools R_user_dir
#' @importFrom data.table fwrite
#'
#' @keywords internal
#' @noRd
.teems_cache <- function(input,
                         file,
                         ext,
                         dir) {
  teems_cache <- tools::R_user_dir(package = "teems", which = "cache")
  object_cache <- file.path(teems_cache, dir)
  if (!dir.exists(paths = object_cache)) {
    dir.create(path = object_cache, recursive = TRUE)
  }
  file_path <- file.path(object_cache, paste(file, ext, sep = "."))

  if (identical(x = ext, y = "tab")) {
  writeLines(text = input,
             con = file_path)
  } else if (identical(x = ext, y = "csv")) {
    data.table::fwrite(input,
                       file = file_path)
  }

  return(file_path)
}
