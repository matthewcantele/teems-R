#' Parse model results
#'
#' @description `teems_parse()` retrieves and processes results from a solved
#'   model run. Results are parsed according to the specified type (variables,
#'   coefficients, or base data). Data validation and consistency checks are
#'   performed during the parsing process.
#'
#'   Learn more about this function including output formats and data structures
#'   in `vignette("something")`
#'
#' @inheritParams teems_solve
#' @param type Character length 1, type of data to parse (default includes all).
#'   Choices:
#'   * `"variable"`: Percentage change values for model variables
#'   * `"coefficient"`: Absolute values for model coefficients
#'   * `"basedata"`: Merged pre- and post-model base data values
#'
#' @importFrom targets tar_load
#' @importFrom purrr pluck
#'
#' @seealso [`teems_solve()`] for running the model simulation.
#'
#' @examples
#' # See `vignette("something")` for examples and explanation
#'
#' @return A list containing the parsed model results according to the specified
#'   type.
#' @export
teems_parse <- function(cmf_path,
                        type = c("variable", "coefficient", "set"),
                        merge_premodel = FALSE) {
call <- match.call()
type <- rlang::arg_match(arg = type)
paths <- .get_postmodel_paths(cmf_path = cmf_path)
sets <- .check_sets(var_paths = paths[["var"]],
                    model_dir = paths[["model"]],
                    call = call)
int <- .check_intertemporal(launchpad_dir = paths[["launchpad"]],
                            model_dir = paths[["model"]],
                            sets = sets[["postmodel"]])
output <- .retrieve_output(type = type,
                           paths = paths,
                           call = call)


if (identical(x = type, y = "basedata")) {
  targets::tar_load(name = final.base_tib, store = file.path(model_dir, "store"))
  output <- .merge_data(
    pre_coeff = final.base_tib[["dt"]],
    post_coeff = output,
    sets = final.set_tib,
    coeff_extract = coeff_extract,
    reference_year = metadata[["reference_year"]],
    intertemporal = intertemporal
  )
  }
output
}
