#' Load and consolidate closure
#'
#' @description `teems_closure()` is a core function that establishes
#'   the direction of any swaps and consolidates swaps and shocks. It
#'   also loads any user-provided closure file allowing for
#'   specification of endogenous/exogenous variable status. Full
#'   variable swaps can be directly specified here or passed in as
#'   output from [`teems_swap()`].
#'
#' @param swap_in Character of length 1 (default is `NULL`) or in the
#'   case of a swap, output of [`teems_swap()`]. Multiple swaps "in"
#'   should be provided in the form of a list of objects created with
#'   [`teems_swap()`].
#' @param swap_out Character of length 1 (default is `NULL`) or in the
#'   case of a swap, output of [`teems_swap()`]. Multiple swaps "out"
#'   should be provided in the form of a list of objects created with
#'   [`teems_swap()`].
#' @param shock An object or in the case of multiple shocks, list of
#'   objects produced by [`teems_shock()`] (default is `NULL`). In the
#'   case of `NULL`, a `NULL` shock will be carried out which
#'   effectively returns the base data.
#' @param closure_file Character of length 1, file name in working
#'   directory or path to a closure file.
#' @param shock_file Character of length 1, file name in working
#'   directory or path to a csv representing the final shock file. No
#'   checks or modifications will be conducted on this file.
#'
#' @return A list of closure configuration options.
#'
#' @details `teems_closure()` return values have no purpose used in
#'   isolation and are rather combined with user inputs in other
#'   `teems` package functions within [`teems_deploy()`] to produce a
#'   path-dependent pipeline resulting in solver-ready input files for
#'   [`teems_solve()`].
#'
#' @seealso [`teems_swap()`] for loading both simple full variable swaps as well as element-specific swaps.
#' @seealso [`teems_shock()`] for loading shocks.
#' @seealso [`teems_deploy()`] for loading the output of this
#'   function.
#'
#' @examples
#' # See `vignette("something")` for examples and explanation.
#'
#' temp_dir <- tools::R_user_dir(package = "teems", "cache")
#' if (!dir.exists(temp_dir)) {
#'   dir.create(temp_dir)
#' }
#' file.copy(from = c(teems_example(path = "user_closure.cls")),
#'           to = temp_dir)
#'
#' # Simple single variable (length 1 for both directions) swaps can
#' # be directly specified here without the need to use teems_swap().
#' qfd_shk <- teems_shock(var = "y",
#'                        type = "uniform",
#'                        value = 0.5)
#'
#' simple_swap <- teems_closure(swap_in = "qfd",
#'                              swap_out = "tfd",
#'                              shock = qfd_shk)
#'
#' # For more complex swaps teems_swap() must be used.
#' qfd_row_in <- teems_swap(var = "qfd",
#'                          type = "uniform",
#'                          value = 0.5,
#'                          REGs = "row")
#'
#' tfd_row_out <- teems_swap(var = "tfd",
#'                           type = "uniform",
#'                           value = 0.5,
#'                           REGs = "row")
#'
#' qfd_row_shk <- teems_shock(var = "qfd",
#'                            type = "uniform",
#'                            value = 0.5)
#'
#' \dontrun{
#' complex_swap <- teems_closure(swap_in = qfd_row_in,
#'                               swap_out = tfd_row_out,
#'                               shock = qfd_row_shk)
#' }
#'
#' # teems_swap() must also be used in the case of multiple swaps in
#' # the same direction. Multiple swaps and shocks must be loaded as a
#' # list.
#' y_in <- teems_swap(var = "y")
#' incomeslack_out <- teems_swap(var = "incomeslack")
#'
#' y_shk <- teems_shock(var = "y",
#'                      type = "uniform",
#'                      value = 2)
#'
#' \dontrun{
#' complex_multi <- teems_closure(swap_in = list(qfd_row_in, y_in),
#'                                swap_out = list(tfd_row_out, incomeslack_out),
#'                                shock = list(qfd_row_shk, y_shk))
#' }
#'
#' # Users can load their own closure file with `NULL` swaps to use as
#' # is or conduct swaps on the loaded file.
#' readLines(file.path(temp_dir, "user_closure.cls"))
#'
#' \dontrun{
#' user_closure <- teems_closure(shock = y_shk,
#'                               closure_file = file.path(temp_dir, "user_closure.cls"))
#' }
#'
#' unlink(x = temp_dir, recursive = TRUE)
#'
#' @export
teems_closure <- function(swap_in = NULL,
                          swap_out = NULL,
                          shock = NULL,
                          closure_file = NULL,
                          shock_file = NULL)
{
if (is.character(x = substitute(expr = swap_in)) && !is.null(x = swap_in)) {
swap_in <- .convert_swap(swap = swap_in)
fun_call <- match.call()
fun_call[["swap_in"]] <- as.name(x = "swap_in")
}
if (is.character(x = substitute(expr = swap_out)) && !is.null(x = swap_out)) {
swap_out <- .convert_swap(swap = swap_out)
  if (!exists(x = "cls_call")) {
  fun_call <- match.call()
  }
fun_call[["swap_out"]] <- as.name(x = "swap_out")
}
if (!exists(x = "fun_call")) {
  fun_call <- match.call()
}
args_list <- as.list(.match_call(fun_call = fun_call,
                                 eval_args = c("swap_in",
                                               "swap_out",
                                               "shock"))[-1])
if (is.null(x = closure_file)) {
message("The standard model-specific closure will be used.")
} else if (!file.exists(closure_file)) {
  stop("The closure specified has not been found.")
} else {
  args_list[["closure_file"]] <- path.expand(path = closure_file)
}
if (!is.null(x = shock_file)) {
  if (!file.exists(shock_file)) {
    stop("The `shock_file` specified has not been found.")
  }
  if (!is.null(x = shock)) {
    stop("A final `shock_file` was provided therefore no additional shocks or modifications are accepted via `shock`.")
  }
  args_list[["shock_file"]] <- path.expand(path = shock_file)
}
args_list
}
