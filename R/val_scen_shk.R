#' @keywords internal
#' @noRd
.val_scen_shk <- function(shock,
                          call) {
  
  shock <- .val_cst_scen_shk(shock = shock,
                             call = call)
  
  return(shock)
}
