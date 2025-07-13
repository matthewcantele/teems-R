#' @noRd
#' @keywords internal
options_class <- R6::R6Class(
  classname = "teems_option",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    # Option fields (start as NULL)
    verbose = NULL,
    ndigits = NULL,
    check_shock_status = NULL,

    # Initialize method
    initialize = function(verbose = NULL, ndigits = NULL, check_shock_status = NULL) {
      self$verbose <- verbose
      self$ndigits <- ndigits
      self$check_shock_status <- check_shock_status
    },
    
    # Export all options as a list
    export = function() {
      list(
        verbose = self$get_verbose(),
        ndigits = self$get_ndigits(),
        check_shock_status = self$get_check_shock_status()
      )
    },
    
    # Import options from a list
    import = function(list) {
      self$set_verbose(list$verbose)
      self$set_ndigits(list$ndigits)
      self$set_check_shock_status(list$check_shock_status)
    },
    
    # Reset all options to NULL
    reset = function() {
      self$verbose <- NULL
      self$ndigits <- NULL
      self$check_shock_status <- NULL
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

    # Validation methods
    validate_verbose = function(verbose) {
      if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        stop("verbose must be TRUE or FALSE")
      }
    },
    
    validate_ndigits = function(ndigits) {
      if (!is.numeric(ndigits)) {
        stop("verbose must be numeric")
      }
    },
    
    validate_check_shock_status = function(check_shock_status) {
      if (!is.logical(check_shock_status) || length(check_shock_status) != 1 || is.na(check_shock_status)) {
        stop("check_shock_status must be TRUE or FALSE")
      }
    },

    # Validate all current options
    validate = function() {
      self$validate_verbose(self$get_verbose())
      self$validate_ndigits(self$get_ndigits())
      self$validate_check_shock_status(self$get_check_shock_status())
    }
  )
)

# Helper operator (like targets uses %|||%)
`%|||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Create constructor functions
options_new <- function(verbose = NULL, ndigits = NULL, check_shock_status = NULL) {
  options_class$new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status
  )
}

options_init <- function(verbose = NULL, ndigits = NULL, check_shock_status = NULL) {
  options_new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status
  )
}

# Create the global options object (like tar_options)
teems_options <- options_init()

# User-facing functions (like tar_option_set)
#' @export
teems_option_set <- function(...) {
  dots <- list(...)
  
  # Validate that all arguments are named
  if (length(dots) > 0 && (is.null(names(dots)) || any(names(dots) == ""))) {
    stop("All arguments must be named")
  }
  
  # Validate argument names
  valid_names <- c("verbose", "ndigits", "check_shock_status")
  invalid_names <- setdiff(names(dots), valid_names)
  if (length(invalid_names) > 0) {
    stop("Invalid option names: ", paste(invalid_names, collapse = ", "))
  }
  
  # Set options using the setter methods
  if ("verbose" %in% names(dots)) {
    teems_options$set_verbose(dots$verbose)
  }

  if ("ndigits" %in% names(dots)) {
    teems_options$set_ndigits(dots$ndigits)
  }
  
  if ("check_shock_status" %in% names(dots)) {
    teems_options$set_check_shock_status(dots$check_shock_status)
  }
  
    # Validate all options after setting
  teems_options$validate()
  
  invisible(NULL)
}

# Get current options (like tar_option_get)
#' @export
teems_option_get <- function(name = NULL) {
  if (is.null(name)) {
    # Return all options
    return(teems_options$export())
  }
  
  # Return specific option
  switch(name,
         verbose = teems_options$get_verbose(),
         ndigits = teems_options$get_ndigits(),
         check_shock_status = teems_options$get_check_shock_status(),
         stop("Unknown option: ", name)
  )
}

# Reset options (like tar_option_reset)
#' @export
teems_option_reset <- function() {
  teems_options$reset()
  invisible(NULL)
}
