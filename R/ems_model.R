#' Load general model specifications
#'
#' @description `ems_model()` loads general model specifications,
#'   conducts pre-pipeline checks, and determines temporal
#'   dynamics. The output of this function is a required input to
#'   the `"model_config"` argument within the [`ems_deploy()`]
#'   function.
#'
#' @param tab_file Character of length 1, path to a local Tablo
#'   model file or selection of an internal Tablo file. See
#'   \href{https://github.com/teems-org/teems-models}{teems-models}
#'   for internally available Tablo files as well as compatible
#'   Tablo file formatting.
#' @param var_omit A character vector (default is `NULL`).
#'   Variables that to be substituted with `0` within the Tablo
#'   model file and removed from the designated closure (if
#' present).
#'
#' @return A tibble contained the parsed Tablo model file for
#'   input to the `"model"` argument of [`ems_deploy()`].
#' 
#' @details `ems_model()` return values have no purpose used in
#'   isolation and
#'   are rather combined with user inputs in other `teems` package functions
#'   within [`ems_deploy()`] to produce a path-dependent pipeline resulting in
#'   solver-ready input files for [`ems_solve()`].
#'   
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function.
#' 
#' @examples
#' # v6.2 format model with variable omission
#' model <- ems_model(
#' tab_file = "GTAPv6.2",
#' var_omit = c(
#'   "atall",
#'   "tfd",
#'   "avaall",
#'   "tf",
#'   "tfm",
#'   "tgd",
#'   "tgm",
#'   "tpd",
#'   "tpm"
#' ))
#' 
#' # v7.0 format model with variable omission
#' model <- ems_model(
#'   tab_file = "GTAP-REv1",
#'   var_omit = c(
#'     "atall",
#'     "avaall",
#'     "tfe",
#'     "tfm",
#'     "tgd",
#'     "tgm",
#'     "tid",
#'     "tim"))
#'     
#' @export
ems_model <- function(tab_file,
                      var_omit = NULL)
{
  # add "auto_omit" arg and other substitution/replacement options
if (missing(tab_file)) {.cli_missing(tab_file)}
args_list <- mget(names(formals()))
call <- match.call()
v <- .validate_model_args(a = args_list,
                          call = call)
model <- .process_tablo(tab_file = v$tab_file,
                        var_omit = v$var_omit,
                        call = call)
model
}
