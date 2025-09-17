#' options
#' @description placeholder
#'
#' @param verbose Logical of length 1 (default is `TRUE`). If
#'   `FALSE`, function-specific diagnostics are silenced.
#' @param ndigits Integer (default is `6`). Exact number of
#'   digits to the right of the decimal point to be written to
#'   file for numeric type double (GEMPack equivalent "real").
#'   This value is passed to the `format()` nsmall argument and
#'   `round()` digits argument.
#' @param check_shock_status Logical of length 1 (default is
#'   `TRUE`). If `FALSE`, no check on shock element
#'   endogenous/exogenous status is conducted.
#' @param post_set_check Logical of length 1 (default is `TRUE`).
#'   If `FALSE`, no post model check is conducted between the
#'   sets as written out from the Tablo file and solver set
#'   binaries. If a Tablo file with no set writeout is used
#'   (e.g., custom file in in-situ solve mode), this option must
#'   be set to `FALSE`.
#' @param timestep_header A character vector length 1 (default is
#'   `"YEAR"`). Coefficient containing a numeric vector of
#' timestep intervals. For novel intertemporal models - modify
#' with caution.
#' @param n_timestep_header A character vector length 1 (default
#'   is `"NTSP"`). Coefficient containing a numeric vector length
#'   one with sum of timestep intervals. For novel intertemporal
#'   models - modify with caution.
#' @param full_exclude A character vector (default is `c("DREL",
#'   "DVER", "XXCR", "XXCD", "XXCP", "SLUG", "EFLG")`). Specifies
#'   headers to fully exclude from all aspects of the model run.
#'   Failure to designate these headers properly will result in
#'   errors because some headers cannot be aggregated or mapped.
#'   Modify with caution.
#' @param docker_tag Character length 1 (default `"latest"`).
#'   Docker tag to specify the which Docker image is use.
#' @param margin_sectors placeholder
#' @param accuracy_threshold Numeric length 1 (default 0.8),
#'   converted to a percentage. 4 digit precision will be
#'   compared against this threshold, generating a warning if it
#'   is not met.
#' @export
ems_option_set <- function(...) {
  dots <- list(...)
  
  # Validate that all arguments are named
  if (length(dots) > 0 && (is.null(names(dots)) || any(names(dots) == ""))) {
    stop("All arguments must be named")
  }
  
  # Validate argument names
  valid_names <- c("verbose", "ndigits", "check_shock_status", "timestep_header", "n_timestep_header", "full_exclude", "docker_tag", "margin_sectors", "accuracy_threshold")
  invalid_names <- setdiff(names(dots), valid_names)
  if (length(invalid_names) > 0) {
    stop("Invalid option names: ", paste(invalid_names, collapse = ", "))
  }
  
  # Set options using the setter methods
  if ("verbose" %in% names(dots)) {
    ems_options$set_verbose(dots$verbose)
  }
  
  if ("ndigits" %in% names(dots)) {
    ems_options$set_ndigits(dots$ndigits)
  }
  
  if ("check_shock_status" %in% names(dots)) {
    ems_options$set_check_shock_status(dots$check_shock_status)
  }
  
  if ("post_set_check" %in% names(dots)) {
    ems_options$set_post_set_check(dots$post_set_check)
  }
  
  if ("timestep_header" %in% names(dots)) {
    ems_options$set_timestep_header(dots$timestep_header)
  }
  
  if ("n_timestep_header" %in% names(dots)) {
    ems_options$set_n_timestep_header(dots$n_timestep_header)
  }
  
  if ("full_exclude" %in% names(dots)) {
    ems_options$set_full_exclude(dots$full_exclude)
  }
  
  if ("docker_tag" %in% names(dots)) {
    ems_options$set_docker_tag(dots$docker_tag)
  }
  
  if ("margin_sectors" %in% names(dots)) {
    ems_options$set_margin_sectors(dots$margin_sectors)
  }
  
  if ("accuracy_threshold" %in% names(dots)) {
    ems_options$set_accuracy_threshold(dots$accuracy_threshold)
  }
  
  # Validate all options after setting
  ems_options$validate()
  
  invisible(NULL)
}

# Get current options (like tar_option_get)
#' @export
ems_option_get <- function(name = NULL) {
  if (is.null(name)) {
    # Return all options
    return(ems_options$export())
  }
  
  # Return specific option
  switch(name,
         verbose = ems_options$get_verbose(),
         ndigits = ems_options$get_ndigits(),
         check_shock_status = ems_options$get_check_shock_status(),
         post_set_check = ems_options$get_post_set_check(),
         timestep_header = ems_options$get_timestep_header(),
         n_timestep_header = ems_options$get_n_timestep_header(),
         full_exclude = ems_options$get_full_exclude(),
         docker_tag = ems_options$get_docker_tag(),
         margin_sectors = ems_options$get_margin_sectors(),
         accuracy_threshold = ems_options$get_accuracy_threshold(),
         stop("Unknown option: ", name)
  )
}

# Reset options (like tar_option_reset)
#' @export
ems_option_reset <- function() {
  ems_options$reset()
  invisible(NULL)
}
