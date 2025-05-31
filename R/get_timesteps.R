#' @importFrom purrr compact pluck
#' 
#' @keywords internal
#' @noRd
.get_timesteps <- function(aux_data,
                           intertemporal) {
  if (intertemporal) {
    aux_data <- qs2::qs_read(file = aux_data)
    n_timestep_header <- names(x = purrr::compact(.x = sapply(
      X = aux_data,
      FUN = attr,
      which = "n_timestep_coeff"
    )))
    n_timestep_coeff <- attr(x = aux_data[[n_timestep_header]], which = "n_timestep_coeff")
    n_timestep <- purrr::pluck(.x = aux_data, n_timestep_header, "data")
    attr(x = n_timestep, "n_timestep_coeff") <- n_timestep_coeff
  } else {
    n_timestep <- NA
  }

  return(n_timestep)
}