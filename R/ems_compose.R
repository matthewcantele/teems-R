#' Parse model results
#'
#' @description `ems_compose()` retrieves and processes results from a solved
#'   model run. Results are parsed according to the specified type (variables,
#'   coefficients, or base data). Data validation and consistency checks are
#'   performed during the parsing process.
#'
#'   Learn more about this function including output formats and data structures
#'   in `vignette("something")`
#'
#' @inheritParams ems_solve
#' @param type Character length 1, type of data to parse (default includes all).
#'   Choices:
#'   * `"variable"`: Percentage change values for model variables
#'   * `"coefficient"`: Absolute values for model coefficients
#'   * `"set"`: Model sets
#'   * `"basedata"`: Merged pre- and post-model basedata values
#'
#' @importFrom rlang arg_match
#'
#' @seealso [`ems_solve()`] for running the model simulation.
#'
#' @examples
#' # See `vignette("something")` for examples and explanation
#'
#' @return A list containing the parsed model results according to the specified
#'   type.
#' @export
ems_compose <- function(cmf_path,
                        type = c("variable", "coefficient", "set", "inputdata"),
                        name = NULL)
{
call <- match.call()
type <- rlang::arg_match(arg = type)
paths <- .get_postmodel_paths(cmf_path = cmf_path,
                              call = call)
comp_extract <- .retrieve_tab_comp(tab_path = paths[["tab"]],
                                   type = type,
                                   call = call)
sets <- .check_sets(var_paths = paths[["var"]],
                    model_dir = paths[["model"]],
                    call = call)
int <- .is_intertemporal(launchpad_dir = paths[["launchpad"]],
                         model_dir = paths[["model"]],
                         #metadata_path = paths[["metadata"]],
                         sets = sets[["postmodel"]])
output <- .retrieve_output(type = type,
                           comp_extract = comp_extract,
                           name = name,
                           paths = paths,
                           sets = sets,
                           int = int,
                           call = call)
output
}
