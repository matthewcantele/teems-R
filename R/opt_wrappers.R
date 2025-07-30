#' @noRd
#' @keywords internal
.o_verbose <- function() {
  ems_option_get("verbose")
}

.o_ndigits <- function() {
  ems_option_get("ndigits")
}

.o_check_shock_status <- function() {
  ems_option_get("check_shock_status")
}

.o_timestep_header <- function() {
  ems_option_get("timestep_header")
}

.o_n_timestep_header <- function() {
  ems_option_get("n_timestep_header")
}

.o_full_exclude <- function() {
  ems_option_get("full_exclude")
}

.o_docker_tag <- function() {
  ems_option_get("docker_tag")
}