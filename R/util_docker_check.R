#' @keywords internal
#' @noRd
.check_docker <- function(image_name) {
  # Check if Docker is installed
  docker_installed <- system("docker --version", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!docker_installed) {
    stop("Docker is not installed on this system.")
  }

  # Check if Docker can be called without sudo
  docker_access <- system("docker info", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!docker_access) {
    stop("Docker is installed but cannot be called without sudo.")
  }

  # Check if the specified image is present
  image_present <- system(paste("docker images -q", shQuote(image_name)), intern = TRUE)
  if (!length(image_present) > 0) {
    stop(paste("Docker is properly configured, but the", dQuote(x = image_name), "image is not present."))
  }

  return(invisible(NULL))
}