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

.o_post_set_check <- function() {
  ems_option_get("post_set_check")
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

.o_margin_sectors <- function() {
  ems_option_get("margin_sectors")
}

.o_accuracy_threshold <- function() {
  ems_option_get("accuracy_threshold")
}