#' Load data specifications
#'
#' @description `teems_base()` loads basedata-specific data and
#'   configurations as well as checks database compatibility. The
#'   output of this function is a required input to the `"base_config"`
#'   argument within the [`teems_deploy()`] function.
#'
#' Learn more about this function including Tablo file limitations in
#' `vignette("something")`
#'
#' @inheritParams teems_model
#'
#' @param dat_har Character of length 1, file name in working
#'   directory or path to a GTAP data HAR file.
#' @param header_rename A named vector (default is `NULL`). Supplies
#'   custom origin and destination matches for database headers that
#'   will supercede any matching default result. The name of each
#'   element will be used to identify the header to rename, and the
#'   value will become the new name. Note that corresponding changes
#'   to the Tablo file are not conducted.
#' @param coefficient_rename A named vector (default is `NULL`).
#'   Supplies custom origin and destination matches for database
#'   coefficients that will supercede any matching default result. The
#'   name of each element will be used to identify the coefficient to
#'   rename, and the value will become the new name. Note that
#'   corresponding changes to the Tablo file are not conducted.
#' @param preagg_data A named vector (default is `NULL`).
#'   Path pair which replaces existing preaggregation (unmodified)
#'   data headers. The name of each element determines the header for
#'   which to replace data. The value is a path to a .csv. The
#'   user-provided data must contain all corresponding sets and
#'   associated elements as in the original data to be replaced, as
#' well as a "Value" column with new values. This replacement will be
#' subject to structure checks and set aggregations.
#' @param postagg_data A named vector (default is `NULL`).
#'   Path pair which replaces existing aggregated data headers. The
#'   user-provided data must contain all aggregation-specific
#'   corresponding sets and associated elements as in the aggregated
#'   data to be replaced as well as a "Value" column with new values.
#'   This replacement will be subject to structure checks according to
#'   selected set aggregations.
#'
#' @return A list of basedata configuration options.
#'
#' @details `teems_base()` return values have no purpose used in
#'   isolation and are rather combined with user inputs in other
#'   `teems` package functions within [`teems_deploy()`] to produce a
#'   path-dependent pipeline resulting in solver-ready input files for
#'   [`teems_solve()`].
#'
#' @seealso [`teems_deploy()`] for loading the output of this
#'   function.
#'
#' @examples
#' # See `vignette("teems-input_files")` for examples and explanation
#'
#' files <- c("gddat.har", "SAVE.csv", "SAVE_preagg.csv", "SAVE_postagg.csv")
#' paths <- sapply(X = files, FUN = teems_example, simplify = FALSE)
#' list2env(x = paths, envir = environment())
#'
#' # Renaming existing data headers and coefficients can be useful
#' # to distinguish from existing variables (e.g., pop) or for header
#' # and coefficient consistency (e.g., VTWR, VTMFSD).
#' base_config <- teems_base(dat_har = gddat.har,
#'                           header_rename = c("POP" = "TPOP"),
#'                           coefficient_rename = c("POP" = "TPOP"))
#'
#' # Users may introduce data here in two ways: pre-aggregation and
#' # post-aggregation. Pre-aggregation data must contain all tuples of
#' # the data that it is to replace, including a `Value` column. Set
#' # names must follow the `teems` convention (see vignette).
#' head(read.csv(SAVE.csv))
#' head(read.csv(SAVE_preagg.csv))
#'
#' base_config <- teems_base(dat_har = gddat.har,
#'                           preagg_data = c("SAVE" = SAVE_preagg.csv))
#'
#' # Post-aggregation data must contain all tuples of the aggregated
#' # data that it is to replace, including a `Value` column.
#'
#' # sample region mapping
#' reg_mapping <- teems_query(component = "mapping",
#'                            database = "v9",
#'                            name = c(REG = "big3"))
#' head(read.csv(postagg_SAVE))
#' head(reg_mapping)
#' all(is.element(el = reg_mapping[["big3"]],
#'                set = read.csv(SAVE_postagg.csv)[["REGr"]]))
#'
#' base_config <- teems_base(dat_har = path_to_har,
#'                           postagg_data = c("SAVE" = SAVE_postagg.csv))
#'
#' # When `verbose` == `TRUE`, the detected GTAP Database version,
#' # reference year, and data format will be printed to the console.
#' base_config <- teems_base(dat_har = gddat.har,
#'                           verbose = TRUE)
#'
#' unlink(x = temp_dir, recursive = TRUE)
#'
#' @export
teems_base <- function(dat_har,
                       header_rename = NULL,
                       coefficient_rename = NULL,
                       preagg_data = NULL,
                       postagg_data = NULL,
                       verbose = FALSE)
{
fun_call <- match.call()
args_list <- as.list(.match_call(fun_call = fun_call)[-1])
if (missing(dat_har)) {
  stop("Argument 'dat_har' is missing.")
} else if (!file.exists(dat_har)) {
    stop("Filepath for 'dat_har' is not found.")
}
metadata <- .get_metadata(con = dat_har)
vetted_versions <- c("v9A", "v10A", "v11c")
if (!is.element(el = metadata[["orig.database.version"]], set = vetted_versions))
  {teems_version <- packageVersion("teems")
   warning(paste("{teems} version:",
                 teems_version,
                 "has only been vetted on GTAP Data Base versions:",
                 toString(x = vetted_versions)))}
if (verbose)
  {
  message(paste("GTAP Data Base version:", metadata[["orig.database.version"]], "\n"),
          paste("Reference year:", metadata[["reference.year"]], "\n"),
          paste("Data format:", metadata[["data.format"]]))}
config <- args_list
config
}
