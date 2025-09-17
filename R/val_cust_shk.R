#' @keywords internal
#' @noRd
.val_cust_shk <- function(shock,
                          call) {
  
  shock <- .val_cst_scen_shk(shock = shock,
                             call = call)

  return(shock)
}
