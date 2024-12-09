#' Load shocks
#'
#' @description `teems_shock()` loads one or more shocks for
#'   processing as well as conducts a series of compatibility checks.
#'   If a shock is specified, the output of this function is a
#'   required input to the `"shock"` argument within the
#'   [`teems_closure()`] function.
#'
#' @param var Character of length 1, the variable to be shocked.
#' @param type Character of length 1, the type of shock. Choices:
#'   * `"uniform"`: a homogenous shock applied to the specified
#'     variable `"var"` or variable elements (using `...`) at the
#'     specified `"value"`.
#'   * `"custom"`: a user-specified granular shock applied to the
#'     specified variable `"var"` or variable elements (using `...`)
#'     according to percentage change values specified in `"file"`.
#'     The interaction of `...` and explicitly defined values in
#'     `"file"` determines the degree of homogeniety in the final
#'     shock.
#'   * `"scenario"`: identical to `"custom"` with the exception that
#'     shock values are provided in absolute terms and constitute a
#'     trajectory that is transformed into percentage change
#'     equivalents according to the specified set mappings.
#'     `"scenario"` shocks must contain all database-specific tuples
#'     (pre-aggregation) for a given variable.
#' @param value Numeric length 1, the value of shocks type
#'   `"uniform"`.
#' @param file Character of length 1, file name in working
#'   directory or path to a csv with shock data.
#' @param ... One or more key-value pairs separated by commas. These
#'   correspond to element-specific shocks
#'
#' @return A list of shock configuration options.
#'
#' @details `teems_shock()` return values have no purpose used in
#'   isolation and are rather combined in [`teems_closure()`] with
#'   user inputs from [`teems_swap()`]. If no shock is specified, a
#'   null shock will be passed resulting in no change to the
#'   underlying base data.
#'
#' @seealso [`teems_closure()`] for loading the output of this
#'   function.
#' @seealso [`teems_swap()`] for changing the standard model closure.
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
#'                    teems_example(path = "custom_shk.csv"),
#'                    teems_example(path = "targeted_shk.csv"),
#'                    teems_example(path = "pop_SSP2.csv")),
#'           to = temp_dir)
#' path_to_har <- file.path(temp_dir, "gdset.har")
#'
#' # Shocks can span the full range from completely heterogenous with
#' # each variable element explicitly assigned a value
#' # (type "custom"), to completely homogenous with a single shock
#' # value applied across an entire variable (type "uniform").
#'
#' # Where specified, set names must conform to the
#' # "mixed_format" style outputted here:
#' teems_query(component = "var",
#'             model = "GTAPv62",
#'             name = "afeall")
#'
#' invisible(teems_sets(set_har = path_to_har,
#'                      region_mapping = "big3",
#'                      sector_mapping = "macro_sector",
#'                      endowment_mapping = "labor_agg",
#'                      verbose = TRUE))
#'
#' ###################################################################
#' # uniform shocks
#'
#' # fully uniform: all variable elements receive the same shock value
#' afeall_full <- teems_shock(var = "afeall",
#'                            type = "uniform",
#'                            value = 2)
#'
#' # partially uniform: applied only to the "chn" region set
#' afeall_chn <- teems_shock(var = "afeall",
#'                           type = "uniform",
#'                           value = 2,
#'                           REGr = "chn")
#'
#' # uniform over select elements: note that multiple elements can be
#' # selected for each set
#' afeall_chn_agri <- teems_shock(var = "afeall",
#'                                type = "uniform",
#'                                value = 2,
#'                                REGr = "chn",
#'                                PROD_COMMj = c("livestock", "crops"),
#'                                TRAD_COMMi = "food")
#'
#' ###################################################################
#' # custom shocks
#'
#' # fully custom: all variable elements receive the specified shock
#' custom_shk <- read.csv(file.path(temp_dir, "custom_shk.csv"))
#' custom_shk
#'
#' afeall_full <- teems_shock(var = "afeall",
#'                            type = "custom",
#'                            file = file.path(temp_dir, "custom_shk.csv"))
#'
#' # fully custom, targeted: a set element receives the specified
#' # shock. Other elements within this set are not shocked. The
#' # targeted set is not present in the loaded shock file.
#' targeted_shk <- read.csv(file.path(temp_dir, "targeted_shk.csv"))
#' targeted_shk
#'
#' # all tuples containing element "usa" within set REGr are shocked
#' afeall_targeted <- teems_shock(var = "afeall",
#'                                type = "custom",
#'                                file = file.path(temp_dir, "targeted_shk.csv"),
#'                                REGr = "usa")
#'
#' # If sets are not present in the data file and are also not
#' # specified in the function, shock values will be uniformly
#' # allocated across all elements in the missing set(s).
#'
#' # all elements in set REGr are uniformly shocked with unique values
#' # from ENDW_COMMi and PROD_COMMj
#' afeall_uni_REGr <- teems_shock(var = "afeall",
#'                                type = "custom",
#'                                file = file.path(temp_dir, "targeted_shk.csv"))
#'
#' ###################################################################
#' # scenario shocks
#' scenario_shk <- read.csv(file.path(temp_dir, "pop_SSP2.csv"))
#' head(scenario_shk)
#' pop_SSP2 <- teems_shock(var = "pop",
#'                         type = "scenario",
#'                         file = file.path(temp_dir, "pop_SSP2.csv"))
#'
#' @export
teems_shock <- function(var,
                        type = c("uniform","custom", "scenario"),
                        value = NULL,
                        file = NULL,
                        ...)
{
if (missing(x = type)) {
  stop("The 'type' argument must be provided for all shocks.")
} else {
  type <- match.arg(type)
}
if (missing(x = var)) {
  stop("The 'var' argument must be provided for all shock types.")
}
if (is.element(el = type, set = c("custom", "scenario"))) {
  if (is.null(x = file)) {
    stop("An 'file' argument must be provided for shock types 'custom' and 'scenario'.")
  } else if (!file.exists(file)) {
      stop("The shock file provided:", file, "does not exist.")
  }
}
if (!missing(x = ...)) {
args_list <- list(var = var,
     type = type,
     value = value,
     file = file,
     shock_set = list(names(x = list(...))),
     shock_ele = list(list(...)))

} else {
args_list <- list(var = var,
                  type = type,
                  value = value,
                  file = file)
}
ls_names <- names(x = args_list)
args_list <- c(args_list, list(ls_names))
args_list
}
