#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  cache_dir <- tools::R_user_dir("teems", "cache")

  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}