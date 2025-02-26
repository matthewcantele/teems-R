#' @keywords internal
#' @noRd
.gather_targets <- function(criteria,
                            envir) {
  targets_ <- rev(ls(envir = envir)[grepl(
    pattern = criteria,
    x = ls(envir = envir)
  )])

  # filter targets_ by class == call so no inadvertent non-class targets are included
  checked_targets_ <- Filter(function(obj) {
    class(x = get(x = obj, envir = envir)) == "call"
  }, targets_)

  # evaluate and list
  ls_targets <- mget(x = checked_targets_, envir = envir)

  return(ls_targets)
}