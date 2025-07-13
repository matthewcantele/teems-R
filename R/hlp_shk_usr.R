#' @noRd
#' @keywords internal
.usr_shk <- function(shock_file) {
  final_shock <- readLines(shock_file)
  class(final_shock) <- "user"
  shock <- list(final_shock)
  class(shock) <- "shock"
  
  shock_list <- list(shocks = shock,
                     shock_file = basename(shock_file))

  return(shock_list)
}