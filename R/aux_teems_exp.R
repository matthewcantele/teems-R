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
teems_example <- function(path = NULL) {
  if (is.null(x = path)) {
    file.path(dir(path = system.file("extdata", package = "teems")))
  } else {
    system.file("extdata", path, package = "teems", mustWork = TRUE)
  }
}
