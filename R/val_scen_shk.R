#' @keywords internal
#' @noRd
.val_scen_shk <- function(args_list,
                          call) {
  
  args_list <- .val_cst_scen_shk(args_list = args_list,
                                 call = call)
  
  return(args_list)
}
