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
#'   according to percentage-change values specified in
#'   `"input"`.
#'   * `"scenario"`: a user-specified granular shock for temporally dynamic
#'   models applied to variable `"var"` and unaggregated tuples
#'   according to values specified in `"input"`. `"scenario"`
#'   shocks must encompass all values associated with tuples
#'   corresponding to unaggregated set elements. Values in
#'   `"input"` must be actual values and will be converted into
#'   percentage-change format.
#' @param input Format contingent on shock `"type"`.
#'   * `"uniform"`: numeric length 1, value of uniform shock.
#'   * `"custom"`: character length 1, path to a csv file; a data frame or data
#'   frame extension (e.g., tibble, data table. Must contain
#'   "Value" as last column representing percentage-change shocks
#'   to each respective tuple.
#'   * `"scenario"`: character length 1, path to a csv file; a data frame or
#'   data frame extension (e.g., tibble, data table. Must contain
#'   a column "Year" corresponding to selected time steps and
#'   "Value" as last column representing an actual value that
#'   will be converted into a percentage-change shock based on
#'   set and time step selection.
#' @param ... One or more key-value pairs separated by commas
#'   corresponding to element-specific uniform shocks.
#'
#' @return A list of shock configuration options.
#'
#' @details `ems_shock()` return values have no purpose used in
#'   isolation and are rather loaded in [`ems_deploy()`]. If no
#'   shock is specified, a null shock will be passed resulting in
#'   no change to the underlying base data.
#'
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function.
#' @seealso [`ems_swap()`] for changing the standard model
#'   closure.
#' 
#' @examples
#' #' # See \href{https://github.com/teems-org/teems-scripts}{teems-scripts}
#' # for the full range of shock types.
#' 
#' # S3 method for type 'uniform'
#' # fully uniform: all variable elements receive the same shock value
#' afeall_full <- ems_shock(var = "afeall",
#'                          type = "uniform",
#'                          value = 2)
#'
#' # partially uniform: applied only to the "chn" element in set REGr (REG)
#' # Note that set designations must consiste of the concatenation of the
#' # standard set (e.g., REG) and variable-specific index (e.g., r).
#' afeall_chn <- ems_shock(var = "afeall",
#'                         type = "uniform",
#'                         REGr = "chn",
#'                         value = 2)
#'
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
