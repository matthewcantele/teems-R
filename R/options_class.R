#' @noRd
#' @keywords internal
options_class <- R6::R6Class(
  classname = "ems_option",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    # Option fields (start as NULL)
    verbose = NULL,
    ndigits = NULL,
    check_shock_status = NULL,
    timestep_header = NULL,
    n_timestep_header = NULL,
    full_exclude = NULL,
    docker_tag = NULL,

    # Initialize method
    initialize = function(verbose = NULL, ndigits = NULL, check_shock_status = NULL, timestep_header = NULL, n_timestep_header = NULL, full_exclude = NULL, docker_tag = NULL) {
      self$verbose <- verbose
      self$ndigits <- ndigits
      self$check_shock_status <- check_shock_status
      self$timestep_header <- timestep_header
      self$n_timestep_header <- n_timestep_header
      self$full_exclude <- full_exclude
      self$docker_tag <- docker_tag
    },
    
    # Export all options as a list
    export = function() {
      list(
        verbose = self$get_verbose(),
        ndigits = self$get_ndigits(),
        check_shock_status = self$get_check_shock_status(),
        timestep_header = self$get_timestep_header(),
        n_timestep_header = self$get_n_timestep_header(),
        full_exclude = self$get_full_exclude(),
        docker_tag = self$get_docker_tag(),
      )
    },
    
    # Import options from a list
    import = function(list) {
      self$set_verbose(list$verbose)
      self$set_ndigits(list$ndigits)
      self$set_check_shock_status(list$check_shock_status)
      self$set_timestep_header(list$timestep_header)
      self$set_n_timestep_header(list$n_timestep_header)
      self$set_full_exclude(list$full_exclude)
      self$set_docker_tag(list$docker_tag)
    },
    
    # Reset all options to NULL
    reset = function() {
      self$verbose <- NULL
      self$ndigits <- NULL
      self$check_shock_status <- NULL
      self$timestep_header <- NULL
      self$n_timestep_header <- NULL
      self$full_exclude <- NULL
      self$docker_tag <- NULL
    },
    
    # Getter methods with defaults (using %|||% operator)
    get_verbose = function() {
      self$verbose %|||% TRUE
    },
    
    get_ndigits = function() {
      self$ndigits %|||% 6L
    },
    
    get_check_shock_status = function() {
      self$check_shock_status %|||% TRUE
    },
    
    get_timestep_header = function() {
      self$timestep_header %|||% "YEAR"
    },
    
    get_n_timestep_header = function() {
      self$n_timestep_header %|||% "NTSP"
    },
    
    get_full_exclude = function() {
      self$full_exclude %|||% c("DREL", "DVER", "XXCR", "XXCD", "XXCP", "SLUG", "EFLG")
    },
    
    get_docker_tag = function() {
      self$docker_tag %|||% "latest"
    },
    
    # Setter methods with validation
    set_verbose = function(verbose) {
      if (!is.null(verbose)) {
        self$validate_verbose(verbose)
      }
      self$verbose <- verbose
    },
    
    set_ndigits = function(ndigits) {
      if (!is.null(ndigits)) {
        self$validate_ndigits(ndigits)
      }
      self$ndigits <- ndigits
    },
    
    set_check_shock_status = function(check_shock_status) {
      if (!is.null(check_shock_status)) {
        self$validate_check_shock_status(check_shock_status)
      }
      self$check_shock_status <- check_shock_status
    },
    
    set_timestep_header = function(timestep_header) {
      if (!is.null(timestep_header)) {
        self$validate_timestep_header(timestep_header)
      }
      self$timestep_header <- timestep_header
    },

    set_n_timestep_header = function(n_timestep_header) {
      if (!is.null(n_timestep_header)) {
        self$validate_n_timestep_header(n_timestep_header)
      }
      self$n_timestep_header <- n_timestep_header
    },
    
    set_full_exclude = function(full_exclude) {
      if (!is.null(full_exclude)) {
        self$validate_full_exclude(full_exclude)
      }
      self$full_exclude <- full_exclude
    },
    
    set_docker_tag = function(docker_tag) {
      if (!is.null(docker_tag)) {
        self$validate_docker_tag(docker_tag)
      }
      self$docker_tag <- docker_tag
    },
    
    # Validation methods
    validate_verbose = function(verbose) {
      if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        cli::cli_abort("{.arg verbose} must be TRUE or FALSE.")
      }
    },
    
    validate_ndigits = function(ndigits) {
      if (!is.numeric(ndigits)) {
        cli::cli_abort("{.arg ndigits} must be numeric.")
      }
    },
    
    validate_check_shock_status = function(check_shock_status) {
      if (!is.logical(check_shock_status) || length(check_shock_status) != 1 || is.na(check_shock_status)) {
        cli::cli_abort("{.arg check_shock_status} must be TRUE or FALSE.")
      }
    },
    
    validate_timestep_header = function(timestep_header) {
      if (!is.character(timestep_header) || !toupper(timestep_header) %=% timestep_header) {
        cli::cli_abort("{.arg timestep_header} must be an upper case character vector.")
      }
    },
    
    validate_n_timestep_header = function(n_timestep_header) {
      if (!is.character(n_timestep_header) || !toupper(n_timestep_header) %=% n_timestep_header) {
        cli::cli_abort("{.arg n_timestep_header} must be an upper case character vector.")
      }
    },

    validate_full_exclude = function(full_exclude) {
      if (!is.character(full_exclude)) {
        cli::cli_abort("{.arg full_exclude} must be a character vector.")
      }
    },
    
    validate_docker_tag = function(docker_tag) {
      if (!is.character(docker_tag)) {
        cli::cli_abort("{.arg docker_tag} must be a character vector.")
      }
    },
    
    # Validate all current options
    validate = function() {
      self$validate_verbose(self$get_verbose())
      self$validate_ndigits(self$get_ndigits())
      self$validate_check_shock_status(self$get_check_shock_status())
      self$validate_timestep_header(self$get_timestep_header())
      self$validate_n_timestep_header(self$get_n_timestep_header())
      self$validate_full_exclude(self$get_full_exclude())
      self$validate_docker_tag(self$get_docker_tag())
    }
  )
)

# fix cli and call forwarding

# Create constructor functions
options_new <- function(verbose = NULL, ndigits = NULL, check_shock_status = NULL, timestep_header = NULL, n_timestep_header = NULL, full_exclude = NULL, docker_tag = NULL) {
  options_class$new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude,
    docker_tag = docker_tag
  )
}

options_init <- function(verbose = NULL, ndigits = NULL, check_shock_status = NULL, timestep_header = NULL, n_timestep_header = NULL, full_exclude = NULL, docker_tag = NULL) {
  options_new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude,
    docker_tag = docker_tag
  )
}

# Create the global options object (like tar_options)
ems_options <- options_init()

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
#' @param timestep_header A character vector length 1 (default is
#'   `"YEAR"`). Coefficient containing a numeric vector of
#'   timestep intervals. For novel intertemporal models - modify
#'   with caution.
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
#' @export
ems_option_set <- function(...) {
  dots <- list(...)
  
  # Validate that all arguments are named
  if (length(dots) > 0 && (is.null(names(dots)) || any(names(dots) == ""))) {
    stop("All arguments must be named")
  }
  
  # Validate argument names
  valid_names <- c("verbose", "ndigits", "check_shock_status", "timestep_header", "n_timestep_header", "full_exclude", "docker_tag")
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
         timestep_header = ems_options$get_timestep_header(),
         n_timestep_header = ems_options$get_n_timestep_header(),
         full_exclude = ems_options$get_full_exclude(),
         docker_tag = ems_options$get_docker_tag(),
         stop("Unknown option: ", name)
  )
}

# Reset options (like tar_option_reset)
#' @export
ems_option_reset <- function() {
  ems_options$reset()
  invisible(NULL)
}
