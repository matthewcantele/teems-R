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
                        type = c("variable", "coefficient", "basedata")) {
browser()
type <- match.arg(arg = type)
launchpad_dir <- dirname(path.expand(cmf_path))
model_dir <- sub(pattern = "/launchpad", replacement = "", x = launchpad_dir)
if (!dir.exists(model_dir)) {
  stop("Model directory not found.")
}
var_paths <- list.files(
  path = file.path(
    launchpad_dir,
    "out",
    "variables",
    "bin"
  ),
  pattern = "csvs",
  full.names = TRUE
)
coeff_paths <- list.files(
  path = file.path(
    launchpad_dir,
    "out",
    "coefficients"
  ),
  pattern = "csv",
  full.names = TRUE
)
set_union <- .unite_csvs(target = "set_csvs", paths = var_paths)
set_elements <- .match_set_ele(sets_out = set_union, paths = var_paths)
targets::tar_load(name = final.set_tib, store = file.path(model_dir, "store"))
final.set_tib[["name"]] <- toupper(x = final.set_tib[["name"]])
.check_set(premodel = final.set_tib, postmodel = set_elements)
var_union <- .unite_csvs(target = "var_csvs", paths = var_paths)
targets::tar_load(name = tablo_var, store = file.path(model_dir, "store"))
targets::tar_load(name = tablo_coeff, store = file.path(model_dir, "store"))
targets::tar_load(time_coeff, store = file.path(model_dir, "store"))
if (identical(x = type, y = "variable")) {
  output <- .parse_var(
    paths = var_paths,
    var_extract = tablo_var,
    vars = var_union,
    sets = set_elements,
    chron_yrs = purrr::pluck(.x = time_coeff, "dt", "CYRS")
  )
} else if (is.element(el = type, set = c("coefficient", "basedata"))) {
  output <- .parse_coeff(
    paths = coeff_paths,
    coeff_extract = tablo_coeff,
    sets = set_elements,
    chron_yrs = purrr::pluck(.x = time_coeff, "dt", "CYRS")
  )

if (identical(x = type, y = "basedata")) {
  targets::tar_load(name = final.base_tib, store = file.path(model_dir, "store"))
  targets::tar_load(name = metadata, store = file.path(model_dir, "store"))
  if (any(grepl(pattern = "^\\(intertemporal\\)$", x = final.set_tib[["qualifier"]]))) {
    intertemporal <- TRUE
  } else {
    intertemporal <- FALSE
  }
  output <- .merge_data(
    pre_coeff = final.base_tib[["dt"]],
    post_coeff = output,
    sets = final.set_tib,
    tab_coeff = tablo_coeff,
    reference_year = metadata[["reference_year"]],
    intertemporal = intertemporal
  )
  }
}
output
}
