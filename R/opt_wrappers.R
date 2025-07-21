#' @noRd
#' @keywords internal
.o_verbose <- function() {
  eqm_option_get("verbose")
}

.o_ndigits <- function() {
  eqm_option_get("ndigits")
}

.o_check_shock_status <- function() {
  eqm_option_get("check_shock_status")
}

.o_timestep_header <- function() {
  eqm_option_get("timestep_header")
}

.o_n_timestep_header <- function() {
  eqm_option_get("n_timestep_header")
}

.o_full_exclude <- function() {
  eqm_option_get("full_exclude")
}