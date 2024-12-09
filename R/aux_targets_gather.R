#' Gather Targets Based on Criteria
#'
#' This function searches the calling environment for objects that match a given criteria and are of class 'call'. It then evaluates and lists these targets.
#'
#' @param criteria A character string to match against the names of objects in the calling environment.
#'
#' @return A list of objects from the calling environment that match the criteria and are of class 'call'.
#' @keywords internal
.gather_targets <- function(criteria) {

  # use outside environment (where targets are defined)
  calling_env <- parent.frame()

  targets_ <- rev(ls(envir = calling_env)[grepl(
    pattern = criteria,
    x = ls(envir = calling_env)
  )])

  # filter targets_ by class == call so no inadvertent non-class targets are included
  checked_targets_ <- Filter(function(obj) {
    class(x = get(x = obj, envir = calling_env)) == "call"
  }, targets_)

  # evaluate and list
  ls_targets <- mget(x = checked_targets_, envir = calling_env)

  return(ls_targets)
}
