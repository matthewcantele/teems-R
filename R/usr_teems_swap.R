#' Load swaps
#'
#' @description `teems_swap()` is a helper function that loads swaps,
#'   allowing for a change of endogenous/exogenous variable status. If
#'   a swap is specified using this function, the output a required
#'   input to the `"swap_in"` or `"swap_out"` arguments of the
#'   [`teems_closure()`] function, depending on intending direction.
#'
#' @param var Character of length 1, model variable to swap.
#' @param ... One or more key-value pairs separated by commas. These
#'   correspond to element-specific swaps.
#'
#' @return A formatted list of variables to swap including any set-
#'   and element-wise specifications.
#'
#' @details `teems_swap()` return values have no purpose used in
#'   isolation and are rather combined in [`teems_closure()`] with
#'   user inputs from [`teems_shock()`]. The standard model-specific
#'   closure will be used if no swaps are specified.
#'
#' @seealso [`teems_closure()`] for loading the output of this
#'   function.
#' @seealso [`teems_query()`] to check exogenous/endogenous status
#'   within the standard model-specific closure as well as retrieve
#'   model variable details.
#' @seealso [`teems_shock()`] for specification of shocks on exogenous
#'   variables.
#' @seealso [`teems_query()`] for model variable information.
#'
#' @examples
#' # See `vignette("something")` for examples and explanation
#'
#' temp_dir <- tools::R_user_dir(package = "teems", "cache")
#' if (!dir.exists(temp_dir)) {
#'   dir.create(temp_dir)
#' }
#' file.copy(from = c(teems_example(path = "gdset.har"),
#'                    teems_example(path = "usr_mapping.csv")),
#'           to = temp_dir)
#' path_to_har <- file.path(temp_dir, "gdset.har")
#'
#' # Swaps can take place across the full variable or specific elements.
#' # Full variable swaps can be directly inputted to teems_closure()
#' # or passed through using teems_swap():
#' gdp_in <- teems_swap(var = "qgdp")
#' tech_out <- teems_swap(var = "aoreg")
#' \dontrun{
#' tech_gdp_cls <- teems_closure(swap_in = gdp_in,
#'                               swap_out = tech_out)
#' }
#'
#' # Element-specific swap
#' # Variable information including set names can be checked with
#' # teems_query(). Set names must conform to the "mixed_format" style
#' # here:
#' teems_query(component = "var",
#'             model = "GTAPv6.2",
#'             name = "qfd")
#'
#' invisible(teems_sets(set_har = path_to_har,
#'                      region_mapping = "big3",
#'                      sector_mapping = "macro_sector",
#'                      endowment_mapping = "labor_agg",
#'                      verbose = TRUE))
#'
#' us_food_q_in <- teems_swap(TRAD_COMMi = "food",
#'                            REGs = "usa",
#'                            var = "qfd")
#'
#' us_food_t_out <- teems_swap(TRAD_COMMi = "food",
#'                             REGs = "usa",
#'                             var = "tfd")
#'
#' # Swap direction is specified within teems_closure()
#' \dontrun{
#' us_food_cls <- teems_closure(swap_in = us_food_q_in,
#'                              swap_out = us_food_t_out)
#' }
#'
#' unlink(x = temp_dir, recursive = TRUE)
#' @export
teems_swap <- function(var,
                       ...)
{
if (!missing(x = ...)) {
  swap_ele <- list(...)
  swap_sets <- names(x = swap_ele)
  } else {
  swap_ele <- list(NA)
  swap_sets <- NA
}
swap <- list(var = var,
             swap_sets = swap_sets,
             swap_ele = swap_ele)
swap_names <- names(x = swap)
swap <- c(swap, list(swap_names))
attr(x = swap, which = "swap") <- TRUE
swap
}
