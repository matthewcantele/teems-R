#' @keywords internal
#' @noRd
.retrieve_cmf <- function(file,
                          cmf_path) {
  docker_path <- grep(
    pattern = file,
    readLines(cmf_path),
    value = TRUE
  )

  file_entry <- sub("/opt/teems", dirname(cmf_path), docker_path)
  file_name <- strsplit(file_entry, " ")[[1]][2]
  file_name <- gsub(
    pattern = "\"|;",
    replacement = "",
    x = file_name
  )

  return(file_name)
}
