#' Load parameter specifications
#'
#' @description `teems_param()` loads parameter-specific data and
#'   configurations as well as binary parameter specifications for
#'   static models. The output of this function is a required value
#'   for the [teems_write()] `"param_config"` argument.
#'
#' @inheritParams teems_base
#'
#' @param par_har Character of length 1, file name in working
#'   directory or path to a GTAP parameter HAR file.
#' @param RORDELTA Logical of length 1 (default is `TRUE`). RORDELTA
#'   determines the mechanism of allocating investment funds across
#'   regions within static model runs.  When TRUE, investment funds
#'   are allocated across regions to equate the change in the expected
#'   rates of return (i.e., rore(r)).  When FALSE, investment funds
#'   are allocated across regions to maintain the existing composition
#'   of capital stocks.
#'
#' @return A list of parameter configuration options.
#'
#' @details `teems_param()` return values have no purpose used in
#'   isolation and are rather combined with user inputs in other
#'   `teems` package functions within [`teems_deploy()`] to produce a
#'   path-dependent pipeline resulting in solver-ready input files for
#'   [`teems_solve()`].
#'
#' @seealso [`teems_deploy()`] for loading the output of this
#'   function.
#'
#' @examples
#' # See `vignette("something")` for examples and explanation
#'
#' temp_dir <- tools::R_user_dir(package = "teems", "cache")
#' if (!dir.exists(temp_dir)) {
#'   dir.create(temp_dir)
#' }
#' file.copy(from = c(teems_example(path = "gdpar.har"),
#'                    teems_example(path = "SUBP.csv"),
#'                    teems_example(path = "SUBP_preagg.csv"),
#'                    teems_example(path = "SUBP_postagg.csv")),
#'           to = temp_dir)
#' path_to_har <- file.path(temp_dir, "gdpar.har")
#'
#' # Renaming existing data headers and coefficients can be useful
#' # for header and coefficient consistency (e.g., ETRE, ETRAE).
#' param_config <- teems_param(par_har = path_to_har,
#'                             coefficient_rename = c("ETRAE" = "ETRE"))
#'
#' # Users may introduce data here in two ways: pre-aggregation and
#' # post-aggregation. Pre-aggregation data must contain all tuples of
#' # the data that it is to replace, including a `Value` column. Set
#' # names must follow the `teems` convention (see vignette).
#' # Preaggregation parameters will be aggregated according to the
#' # default weighted aggregations (see vignette).
#' original_SUBP <- file.path(temp_dir, "SUBP.csv")
#' preagg_SUBP <- file.path(temp_dir, "SUBP_preagg.csv")
#' head(read.csv(original_SUBP))
#' head(read.csv(preagg_SUBP))
#'
#' param_config <- teems_param(par_har = path_to_har,
#'                             preagg_data = c("SUBP" = preagg_SUBP))
#'
#' # Post-aggregation data must contain all tuples of the aggregated
#' # data that it is to replace, including a `Value` column.
#' postagg_SUBP <- file.path(temp_dir, "SUBP_postagg.csv")
#'
#' # sample region mapping
#' reg_mapping <- teems_query(component = "mapping",
#'                            database = "v9",
#'                            name = c(REG = "big3"))
#' sector_mapping <- teems_query(component = "mapping",
#'                               database = "v9",
#'                               name = c(TRAD_COMM = "macro_sector"))
#' head(read.csv(postagg_SUBP))
#' head(reg_mapping)
#' head(sector_mapping)
#' all(is.element(el = reg_mapping[["big3"]],
#'                set = read.csv(postagg_SUBP)[["REGr"]]))
#' all(is.element(el = sector_mapping[["macro_sector"]],
#'                set = read.csv(postagg_SUBP)[["TRAD_COMMi"]]))
#'
#' param_config <- teems_param(par_har = path_to_har,
#'                             postagg_data = c("SUBP" = postagg_SUBP))
#'
#' unlink(x = temp_dir, recursive = TRUE)
#'
#' @export
teems_param <- function(par_har,
                        RORDELTA = TRUE,
                        header_rename = NULL,
                        coefficient_rename = NULL,
                        preagg_data = NULL,
                        postagg_data = NULL)
{
stopifnot(is.logical(x = RORDELTA))
call <- match.call()
args_list <- mget(x = names(x = formals()))
RORDELTA <- as.integer(x = RORDELTA)
args_list[["par_har"]] <- .check_input_file(file = par_har,
                                            ext = "har",
                                            call = call)
config <- args_list
config
}
