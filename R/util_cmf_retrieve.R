#' @keywords internal
#' @noRd
.cmf_retrieve <- function(file,
                          cmf_path,
                          run_dir) {
file_end <- strsplit(
  x = grep(
    pattern = file,
    x = readLines(con = cmf_path),
    value = TRUE
  ),
  split = "launchpad/"
)[[1]][2]

file_name <- sub(pattern = "\";",
                 replacement = "",
                 x = file_end)

file_target <- readLines(con = file.path(run_dir, file_name))
return(file_target)
}
