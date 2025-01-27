.infer_closure <- function(tab_file) {
  internal_tabs <- names(x = internal_tab)
  if (!is.element(el = tab_file, set = internal_tabs)) {
    stop(paste("The closure implied from Tab file:",
               tab_file,
               "is not available.",
               "Internal tabs with associated standard closures include:",
               toString(internal_tabs)))
  } else {
    closure <- tail(head(closures[[tab_file]], -3), -1)
  }
  return(closure)
}
