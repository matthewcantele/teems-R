options_class <- R6::R6Class(
  classname = "teems_option",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    # Option fields (start as NULL)
    verbose = NULL,

    # Initialize method
    initialize = function(verbose = NULL) {
      self$verbose <- verbose
    },
    
    # Export all options as a list
    export = function() {
      list(
        verbose = self$get_verbose()
      )
    },
    
    # Import options from a list
    import = function(list) {
      self$set_verbose(list$verbose)
    },
    
    # Reset all options to NULL
    reset = function() {
      self$verbose <- NULL
    },
    
    # Getter methods with defaults (using %|||% operator)
    get_verbose = function() {
      self$verbose %|||% TRUE
    },
    

    # Setter methods with validation
    set_verbose = function(verbose) {
      if (!is.null(verbose)) {
        self$validate_verbose(verbose)
      }
      self$verbose <- verbose
    },

    # Validation methods
    validate_verbose = function(verbose) {
      if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        stop("verbose must be TRUE or FALSE")
      }
    },

    # Validate all current options
    validate = function() {
      self$validate_verbose(self$get_verbose())
    }
  )
)

# Helper operator (like targets uses %|||%)
`%|||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Create constructor functions
options_new <- function(verbose = NULL) {
  options_class$new(
    verbose = verbose
  )
}

options_init <- function(verbose = NULL) {
  options_new(
    verbose = verbose
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
  valid_names <- c("method", "digits", "verbose", "output_dir")
  invalid_names <- setdiff(names(dots), valid_names)
  if (length(invalid_names) > 0) {
    stop("Invalid option names: ", paste(invalid_names, collapse = ", "))
  }
  
  # Set options using the setter methods
  if ("verbose" %in% names(dots)) {
    teems_options$set_verbose(dots$verbose)
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
         stop("Unknown option: ", name)
  )
}

# Reset options (like tar_option_reset)
#' @export
teems_option_reset <- function() {
  teems_options$reset()
  invisible(NULL)
}

# # Functions that use the options
# my_process_data <- function(data, method = NULL, digits = NULL, verbose = NULL) {
#   # Use provided arguments or fall back to global options
#   method <- method %|||% my_options$get_method()
#   digits <- digits %|||% my_options$get_digits()
#   verbose <- verbose %|||% my_options$get_verbose()
#   
#   if (verbose) {
#     cat("Processing data with method:", method, "and digits:", digits, "\n")
#   }
#   
#   # Your processing logic here
#   result <- switch(method,
#                    "default" = round(mean(data), digits),
#                    "robust" = round(median(data), digits),
#                    "simple" = round(sum(data), digits),
#                    "advanced" = round(var(data), digits)
#   )
#   
#   return(result)
# }
# 
# # Example usage:
# cat("=== Example Usage ===\n")
# 
# # Set global options
# my_option_set(method = "robust", digits = 4, verbose = TRUE)
# 
# # View current options
# print(my_option_get())
# 
# # Use a function that respects the global options
# data <- c(1.23456, 2.34567, 3.45678)
# result1 <- my_process_data(data)
# cat("Result 1:", result1, "\n")
# 
# # Override specific options
# result2 <- my_process_data(data, method = "simple", digits = 1)
# cat("Result 2:", result2, "\n")
# 
# # Reset options
# my_option_reset()
# cat("After reset:", toString(my_option_get()), "\n")