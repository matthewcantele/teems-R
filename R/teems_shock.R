#' Specificy shocks
#' 
#' @importFrom rlang arg_match
#'
#' @description `teems_shock()` is a generic function that loads shocks for
#'   processing as well as conducts a series of compatibility checks. The
#'   accepted values for `"input"` depend on the shock `"type"` specified. If a
#'   shock is to be carried out, the output of this function is a required input
#'   to the `"shock"` argument within the [`teems_model()`] function.
#'
#' @param var Character of length 1, the variable to be shocked.
#' @param type Character of length 1, the type of shock. Choices:
#'   * `"uniform"`: a homogenous shock applied to the specified
#'   variable `"var"` or variable elements (using `...`) at the specified
#'   `"value"`.
#'   * `"custom"`: a user-specified granular shock applied to
#'   variable `"var"` and aggregated set-specific variable tuples according to
#'   percentage-change values specified in `"input"`.
#'   * `"scenario"`: a user-specified granular shock for temporally dynamic
#'   models applied to variable `"var"` and unaggregated tuples according to
#'   values specified in `"input"`. `"scenario"` shocks must encompass all
#'   values associated with tuples corresponding to unaggregated set elements.
#'   Values in `"input"` must be actual values and will be converted into
#'   percentage-change format.
#' @param input Format contingent on shock `"type"`.
#'   * `"uniform"`: numeric length 1.
#'   * `"custom"`: character length 1, path to a csv file or data.frame. Must
#'   contain one column "Value".
#'   * `"scenario"`: character length 1, path to a csv file or data.frame. Must
#'   contain one column "Value" and one column "Year".
#' @param ... One or more key-value pairs separated by commas corresponding to
#'   element-specific shocks
#'
#' @return A list of shock configuration options.
#'
#' @details `teems_shock()` return values have no purpose used in isolation and
#'   are rather loaded in [`teems_model()`]. If no shock is specified, a null
#'   shock will be passed resulting in no change to the underlying base data.
#'
#' @seealso [`teems_model()`] for loading the output of this function.
#' @seealso [`teems_swap()`] for changing the standard model closure.
#' 
#' @examples
#' # uniform shock
#' # fully uniform: all variable elements receive the same shock value
#' afeall_full <- teems_shock(var = "afeall",
#'                            type = "uniform",
#'                            input = 2)
#'
#' # partially uniform: applied only to the "chn" element in set REGr (REG)
#' # Note that set designations must consiste of the concatenation of the
#' # standard set (e.g., REG) and variable-specific index (e.g., r).
#' afeall_chn <- teems_shock(var = "afeall",
#'                           type = "uniform",
#'                           input = 2,
#'                           REGr = "chn")
#'
#' # partially uniform over multiple sets and applied to multiple elements: 
#' # applied to the "chn" element in set REGr, "livestock" and "crops" elements 
#' # in PROD_COMMj and "food" element in TRAD_COMMi.
#' afeall_chn_agri <- teems_shock(var = "afeall",
#'                                type = "uniform",
#'                                input = 2,
#'                                REGr = "chn",
#'                                PROD_COMMj = c("livestock", "crops"),
#'                                TRAD_COMMi = "food")
#'
#' # custom shock
#' df <- expand.grid(ENDW_COMMi = c("labor", "capital", "natlres", "land"),
#'                   PROD_COMMj = c("svces", "food", "crops", "mnfcs", 
#'                   "livestock", "frs", "zcgds"),
#'                   REGr = c("row", "chn", "usa"))
#' df$Value <- runif(nrow(df))
#' head(df)
#' afeall_full <- teems_shock(var = "afeall",
#'                            type = "custom",
#'                            input = df)
#'                            
#' # scenario shock
#' df <- expand.grid(REGr = c("row", "chn", "usa"),
#'                   Year = seq(2015, 2030, 5))
#' df <- df[order(df$REGr),]
#' row_pop <- 5667 + (1:4) * 1.5
#' chn_pop <- 1390 + (1:4) * 0.1
#' usa_pop <- 323 + (1:4) * 0.6
#' df$Value <- c(row_pop, chn_pop, usa_pop)
#' head(df)
#' pop_shk <- teems_shock(var = "pop",
#'                        type = "scenario",
#'                        input = df)
#'
#' @export
teems_shock <- function(var,
                        type = c("uniform", "custom", "scenario"),
                        input,
                        ...)
{
  if (missing(var)) {.cli_missing(var)}
  if (missing(type)) {.cli_missing(type)}
  if (missing(input)) {.cli_missing(input)}
  class(type) <- rlang::arg_match(type)
  UseMethod("teems_shock", type)
}
