#' Load swaps
#'
#' @importFrom uuid UUIDgenerate
#'
#' @description `ems_swap()` is a helper function that loads swaps, allowing
#'   for a change of endogenous/exogenous variable status. If a swap is
#'   specified using this function, the output is a required input to the
#'   `"swap_in"` or `"swap_out"` arguments of the [`teems_model()`] function.
#'
#' @param var Character of length 1, model variable to swap.
#' @param ... One or more key-value pairs separated by commas. These correspond
#'   to element-specific swaps in the format SETi = "element" or SETi =
#'   c("element1", "element2") where i denotes the variable-specific index (see
#'   examples). Sets not specified are implicitly determined as uniform.
#'
#' @return A list with swap specifications.
#'
#' @details `ems_swap()` return values have no purpose used in isolation. The
#'   standard model-specific closure will be used if no swaps are specified.
#'   Note that full variable swaps can be directly inputted as a character
#'   string in [`teems_model()`].
#'
#' @seealso [`teems_query()`] to check exogenous/endogenous status within the
#'   standard model-specific closure as well as retrieve model variable details.
#' @seealso [`teems_shock()`] for specification of shocks on exogenous
#'   variables.
#'
#' @examples
#' # Full variable swaps
#' tfd_out <- ems_swap(var = "tfd")
#' qfd_in <- ems_swap(var = "qfd")
#'
#' # Partial variable swaps (note distinction between "REGr" and "REGs")
#' chn_food_tfd_out <- ems_swap(var = "tfd",
#'                              TRAD_COMMi = "food",
#'                              REGr = "chn")
#'                                
#' chn_food_qfd_in <- ems_swap(var = "qfd",
#'                             TRAD_COMMi = "food",
#'                             REGs = "chn")
#'
#' # Partial variable multiple element swaps
#' usa_multi_tfd_out <- ems_swap(var = "tfd",
#'                               TRAD_COMMi = c("food", "crops"),
#'                               REGr = "usa")
#'                                
#' usa_multi_qfd_in <- ems_swap(var = "qfd",
#'                              TRAD_COMMi = c("food", "crops"),
#'                              REGs = "usa")
#'                               
#' @export
ems_swap <- function(var,
                     ...)
{
if (!missing(...)) {
  swap_ele <- list(...)
  swap_sets <- names(swap_ele)
  } else {
  swap_ele <- list(NA)
  swap_sets <- NA
}
swap <- list(var = var,
             swap_sets = swap_sets,
             swap_ele = swap_ele)
call <- match.call()
swap <- structure(swap,
                  call = call,
                  call_id = uuid::UUIDgenerate(),
                  swap = TRUE)
swap <- list(swap)
swap
}
