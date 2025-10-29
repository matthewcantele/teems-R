#' @keywords internal
#' @noRd
.validate_shock <- function(shock,
                            call) {
  UseMethod(".validate_shock")
} 

#' @method .validate_shock uniform
#' @export
.validate_shock.uniform <- function(shock,
                                    call) {

  checklist <- list(
    var = "character",
    input = "numeric",
    subset = c("NULL", "list")
  )
  
  .check_arg_class(
    args_list = shock,
    checklist = checklist,
    call = call
  )
  
  if (any(lengths(shock$subset) > 1)) {
    depth <- "multi"
  } else {
    depth <- "single"
  }

  shock <- structure(shock,
                     call = call,
                     class = c(depth, "shock", class(shock))
  )
  
  shock <- list(shock)
  return(shock)
} 

#' @method .validate_shock default
#' @export
.validate_shock.default <- function(shock,
                                    call) {
  checklist <- list(
    var = "character",
    input = c("character", "data.frame")
  )

  .check_arg_class(
    args_list = shock,
    checklist = checklist,
    call = call
  )

  shock$input <- .shock_preload(
    input = shock$input,
    type = class(shock)[[1]]
  )

  shock$set <- colnames(shock$input)[!colnames(shock$input) %in% "Value"]
  shock <- structure(shock,
    call = call,
    class = c(class(shock), "shock")
  )
  shock <- list(shock)
  return(shock)
}