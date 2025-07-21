#' @noRd
#' @keywords internal
options_class <- R6::R6Class(
  classname = "eqm_option",
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

    # Initialize method
    initialize = function(verbose = NULL, ndigits = NULL, check_shock_status = NULL, timestep_header = NULL, n_timestep_header = NULL, full_exclude = NULL) {
      self$verbose <- verbose
      self$ndigits <- ndigits
      self$check_shock_status <- check_shock_status
      self$timestep_header <- timestep_header
      self$n_timestep_header <- n_timestep_header
      self$full_exclude <- full_exclude
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
    },
    
    # Reset all options to NULL
    reset = function() {
      self$verbose <- NULL
      self$ndigits <- NULL
      self$check_shock_status <- NULL
      self$timestep_header <- NULL
      self$n_timestep_header <- NULL
      self$full_exclude <- NULL
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
    
    # Validate all current options
    validate = function() {
      self$validate_verbose(self$get_verbose())
      self$validate_ndigits(self$get_ndigits())
      self$validate_check_shock_status(self$get_check_shock_status())
      self$validate_timestep_header(self$get_timestep_header())
      self$validate_n_timestep_header(self$get_n_timestep_header())
      self$validate_full_exclude(self$get_full_exclude())
    }
  )
)

# fix cli and call forwarding

# Create constructor functions
options_new <- function(verbose = NULL, ndigits = NULL, check_shock_status = NULL, timestep_header = NULL, n_timestep_header = NULL, full_exclude = NULL) {
  options_class$new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude
  )
}

options_init <- function(verbose = NULL, ndigits = NULL, check_shock_status = NULL, timestep_header = NULL, n_timestep_header = NULL, full_exclude = NULL) {
  options_new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude
  )
}

# Create the global options object (like tar_options)
eqm_options <- options_init()

#' options
#' @description
#' placeholder
#' 
#' @param full_exclude A character vector (default is `c("DREL", "DVER", "XXCR",
#'   "XXCD", "XXCP", "SLUG", "EFLG")`). Specifies headers to fully exclude from
#'   all aspects of the model run. Failure to designate these headers properly
#'   will result in errors because some headers cannot be aggregated or mapped.
#'   Modify with caution.
#' @export
eqm_option_set <- function(...) {
  dots <- list(...)
  
  # Validate that all arguments are named
  if (length(dots) > 0 && (is.null(names(dots)) || any(names(dots) == ""))) {
    stop("All arguments must be named")
  }
  
  # Validate argument names
  valid_names <- c("verbose", "ndigits", "check_shock_status", "timestep_header", "n_timestep_header", "full_exclude")
  invalid_names <- setdiff(names(dots), valid_names)
  if (length(invalid_names) > 0) {
    stop("Invalid option names: ", paste(invalid_names, collapse = ", "))
  }
  
  # Set options using the setter methods
  if ("verbose" %in% names(dots)) {
    eqm_options$set_verbose(dots$verbose)
  }

  if ("ndigits" %in% names(dots)) {
    eqm_options$set_ndigits(dots$ndigits)
  }
  
  if ("check_shock_status" %in% names(dots)) {
    eqm_options$set_check_shock_status(dots$check_shock_status)
  }
  
  if ("timestep_header" %in% names(dots)) {
    eqm_options$set_timestep_header(dots$timestep_header)
  }
  
  if ("n_timestep_header" %in% names(dots)) {
    eqm_options$set_n_timestep_header(dots$n_timestep_header)
  }
  
  if ("full_exclude" %in% names(dots)) {
    eqm_options$set_full_exclude(dots$full_exclude)
  }
  
    # Validate all options after setting
  eqm_options$validate()
  
  invisible(NULL)
}

# Get current options (like tar_option_get)
#' @export
eqm_option_get <- function(name = NULL) {
  if (is.null(name)) {
    # Return all options
    return(eqm_options$export())
  }
  
  # Return specific option
  switch(name,
         verbose = eqm_options$get_verbose(),
         ndigits = eqm_options$get_ndigits(),
         check_shock_status = eqm_options$get_check_shock_status(),
         timestep_header = eqm_options$get_timestep_header(),
         n_timestep_header = eqm_options$get_n_timestep_header(),
         full_exclude = eqm_options$get_full_exclude(),
         stop("Unknown option: ", name)
  )
}

# Reset options (like tar_option_reset)
#' @export
eqm_option_reset <- function() {
  eqm_options$reset()
  invisible(NULL)
}
