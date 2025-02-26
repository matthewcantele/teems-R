#' Get path to teems example
#'
#' teems comes bundled with some example files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#'
#' @details Function modified from `readxl::readxl_example()`
#'
#' @export
#' @examples
#' teems_example()
#' teems_example("user_model.tab")
teems_example <- function(file) {
  temp_dir <- tools::R_user_dir(package = "teems", which = "cache")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir)
  }

  origin <- system.file("extdata", file, package = "teems", mustWork = TRUE)
  file.copy(from = origin,
            to = temp_dir)

  file.path(temp_dir, file)
}
