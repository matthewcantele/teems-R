#' @keywords internal
#' @noRd
.check_docker <- function(image_name,
                          call) {
  # Check if Docker is installed
  docker_installed <- system("docker --version", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!docker_installed) {
    .cli_abort(action = "abort",
               msg = "Docker is required to call the solver and is not 
               installed on this system.")
  }

  # Check if Docker can be called without sudo
  docker_access <- system("docker info", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!docker_access) {
    .cli_action(action = "abort",
                msg = "Docker is installed but cannot be called without sudo. 
                For Linux users, see instructions here: 
                {.href [Manage Docker as a non-root user](https://docs.docker.com/engine/install/linux-postinstall/)}",
                call = call)
  }

  # Check if the specified image is present
  image_present <- system(paste("docker images -q", shQuote(image_name)), intern = TRUE)
  if (!length(image_present) > 0) {
    .cli_action(action = "abort",
                msg = "The {.val {image_name}} Docker image is not present.",
                call = call)
  }

  return(invisible(NULL))
}