#' @keywords internal
.build_int_sets <- function(set_extract,
                            int_data,
                            reference_year) {
  int_sets <- subset(x = set_extract, subset = intertemporal)
  n_timestep_coeff <- attr(x = int_data, which = "n_timestep_coeff")

  int_sets <- .fill_time_sets(
    n_timestep = int_data[["n_timestep"]],
    n_timestep_coeff = n_timestep_coeff,
    int_sets = int_sets
  )

  CYRS <- reference_year + int_data[["timesteps"]]
  attr(x = int_sets, which = "CYRS") <- CYRS
  return(int_sets)
}