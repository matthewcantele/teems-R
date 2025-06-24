#' @importFrom purrr compact pluck
#' 
#' @keywords internal
#' @noRd
.get_int_data <- function(aux_data,
                          intertemporal) {
  if (intertemporal) {
    aux_data <- qs2::qs_read(file = aux_data)
    n_timestep_header <- attr(x = aux_data, which = "n_timestep_header")
    n_timestep <- purrr::pluck(.x = aux_data, n_timestep_header, "data")
    timestep_header <- attr(x = aux_data, which = "timestep_header")
    timesteps <- purrr::pluck(.x = aux_data, timestep_header, "data")
    int_data <- list(n_timestep = n_timestep,
                     timesteps = timesteps)

    attr(x = int_data, which = "n_timestep_coeff") <- purrr::pluck(.x = aux_data, n_timestep_header, "coefficient")
    attr(x = int_data, which = "timestep_coeff") <- purrr::pluck(.x = aux_data, timestep_header, "coefficient")
  } else {
    int_data <- NA
  }

  return(int_data)
}