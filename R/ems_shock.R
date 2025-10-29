#' Specificy shocks
#'
#' @importFrom rlang arg_match
#'
#' @description `ems_shock()` is a generic function that loads
#'   shocks for processing as well as conducts a series of
#'   compatibility checks. The accepted values for `...` depend
#'   on the shock `"type"` specified. If a shock is to be carried
#'   out, the output of this function is a required input to the
#'   `"shock"` argument within the [`ems_deploy()`] function.
#'
#' @param var Character of length 1, the variable to be shocked.
#' @param type Character of length 1, the type of shock. Choices:
#'   * `"uniform"`: a homogenous shock applied to the specified
#'   variable `"var"` or variable elements (using `...`) at the
#'   specified `"value"`.
#'   * `"custom"`: a user-specified granular shock applied to
#'   variable `"var"` and aggregated set-specific variable tuples
#'   according to percentage-change values.
#'   * `"scenario"`: a user-specified granular shock for temporally dynamic
#'   models applied to variable `"var"` and unaggregated tuples
#'   according to values specified in `"input"`. `"scenario"`
#'   shocks must encompass all values associated with tuples
#'   corresponding to unaggregated set elements. Values in
#' @param ... Additional subsetting options passed to
#'   [`ems_shock.uniform()`].
#'
#' @return A list of shock configuration options.
#'
#' @details `ems_shock()` return values have no purpose used in
#'   isolation and are rather loaded in [`ems_deploy()`]. If no
#'   shock is specified, a null shock will be passed resulting in
#'   no change to the underlying base data.
#'
#' @seealso [`ems_shock.uniform()`] for `"uniform"` shocks.
#' @seealso [`ems_shock.custom()`] for `"custom"` shocks.
#' @seealso [`ems_shock.scenario()`] for `"scenario"` shocks.
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function.
#' @seealso [`ems_swap()`] for changing the standard model
#'   closure.
#' 
#' @examples
#' #' # See \href{https://github.com/teems-org/teems-scripts}{teems-scripts}
#' # for the full range of shock types.
#' @export
ems_shock <- function(var,
                      type = c("uniform", "custom", "scenario"),
                      ...)
{
  if (missing(var)) {.cli_missing(var)}
  if (missing(type)) {.cli_missing(type)}
  class(type) <- rlang::arg_match(type)
  UseMethod("ems_shock", type)
}
