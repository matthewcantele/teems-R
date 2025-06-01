#' @importFrom cli cli_abort
#' 
#' @keywords internal
#' @noRd
.infer_closure <- function(tab_file,
                           quiet,
                           call) {
  pot_internal_tab <- strsplit(x = basename(path = tab_file),
                               split = "\\.tab")[[1]][1]
  
  closure_file <- .check_input(file = pot_internal_tab,
                               valid_ext = "cls",
                               call = call)
  return(closure_file)
}
