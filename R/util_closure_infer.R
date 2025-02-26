#' @importFrom cli cli_abort
#' 
#' @keywords internal
#' @noRd
.infer_closure <- function(tab_file) {
  internal_tabs <- names(x = internal_tab)
  if (!is.element(el = tab_file, set = internal_tabs)) {
    cli::cli_abort(c("x" = "The closure implied from the Tablo file 
                     {.val {tab_file}} is not available.",
                     "i" = "Note that only internally available Tablo files are 
                     linked to standard closures.",
                     "i" = "Internal tabs with associated standard closures 
                     include {.val {internal_tabs}}."))
  } else {
    closure <- internal_cls[[tab_file]]
  }
  return(closure)
}
