#' @keywords internal
#' @noRd
.check_docker <- function(image_name,
                          call) {

  docker_installed <- system("docker --version", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!docker_installed) {
    .cli_action(solve_err$docker_installed,
      action = "abort",
      call = call
    )
  }

  docker_access <- system("docker info", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  if (!docker_access) {
    .cli_action(solve_err$docker_sudo,
      action = "abort",
      url = "https://docs.docker.com/engine/install/linux-postinstall/",
      hyperlink = "Manage Docker as a non-root user",
      call = call
    )
  }

  image_present <- system(paste("docker images -q", shQuote(image_name)), intern = TRUE)
  if (!length(image_present) > 0) {
    .cli_action(solve_err$docker_x_image,
      action = "abort",
      url = "https://github.com/teems-org/teems-solver",
      hyperlink = "teems-solver repository",
      call = call
    )
  }

  return(invisible(NULL))
}