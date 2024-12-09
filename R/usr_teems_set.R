#' Load set specifications
#'
#' @description `teems_sets()` loads set-specific data and specifies
#'   core (i.e., read-in) model sets which include region, sector, and
#'   endowment mappings. The output of this function is a required
#'   input to the `"set_config"` argument within the [`teems_deploy()`]
#'   function.
#'
#' @inheritParams teems_model
#'
#' @param set_har Character of length 1, file name in working
#'   directory or path to a GTAP set HAR file.
#' @param region_mapping Character of length 1, name of internal
#'   region mapping (see [`teems_query()`]) or path to a two-column
#'   csv representing a database-specific region mapping.
#' @param sector_mapping Character of length 1, name of internal
#'   sectorf mapping (see [`teems_query()`]) or path to a two-column
#'   csv representing a database-specific sector mapping.
#' @param endowment_mapping Character of length 1, name of internal
#'   endowment mapping (see [`teems_query()`]) or path to a two-column
#'   csv representing a database-specific endowment mapping.
#'
#' @importFrom purrr map2
#'
#' @return A list of set configuration options.
#'
#' @details `teems_sets()` return values have no purpose used in
#'   isolation and are rather combined with user inputs in other
#'   `teems` package functions within [`teems_deploy()`] to produce a
#'   path-dependent pipeline resulting in solver-ready input files for
#'   [`teems_solve()`].
#'
#' @seealso [`teems_query()`] for available internal set mappings.
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
#' file.copy(from = c(teems_example(path = "gdset.har"),
#'                    teems_example(path = "user_mapping.csv")),
#'           to = temp_dir)
#' path_to_har <- file.path(temp_dir, "gdset.har")
#'
#' # Set mappings can be loaded from internally available defaults,
#' # see [`teems_query()`] or provided in the form of a 2-column csv
#' # by the user. In the latter case, the first column must contain
#' # all set- and database-specific elements and the second column
#' # specifies the respective mapping.
#' head(read.csv(file.path(temp_dir, "usr_mapping.csv")))
#' set_config <- teems_sets(set_har = path_to_har,
#'                          region_mapping = "AR5",
#'                          sector_mapping = file.path(temp_dir, "usr_mapping.csv"),
#'                          endowment_mapping = "labor_agg")
#'
#' # When `verbose` == `TRUE`, the detected GTAP Database version,
#' # reference year, and data format will be printed to the console.
#' set_config <- teems_sets(set_har = path_to_har,
#'                          region_mapping = "AR5",
#'                          sector_mapping = file.path(temp_dir, "usr_mapping.csv"),
#'                          endowment_mapping = "labor_agg",
#'                          verbose = TRUE)
#'
#' unlink(x = temp_dir, recursive = TRUE)
#'
#' @export
teems_sets <- function(set_har,
                       region_mapping,
                       sector_mapping,
                       endowment_mapping,
                       verbose = FALSE)
{
fun_call <- match.call()
args_list <- as.list(.match_call(fun_call = fun_call)[-1])
if (missing(x = set_har)) {
  stop(paste("Argument", dQuote(x = "set_har"), "is missing"))
} else {
  if (!file.exists(set_har)) {
    stop(paste("Filepath for", dQuote(x = "set_har"), "is not found."))
  }
}
database_version <- .get_metadata(con = set_har)[["database.version"]]
map_args <- c("endowment_mapping","region_mapping","sector_mapping")
mappings <- mget(x = map_args)
if (is.element(el = database_version, set = c("v9", "v10"))) {
  endowment_set <- "ENDW_COMM"
  region_set <- "REG"
  tradables_set <- "TRAD_COMM"
} else if (identical(x = database_version, y = "v11")) {
  endowment_set <- "ENDW"
  region_set <- "REG"
  tradables_set <- "COMM"
}
ls_set_ele <- purrr::map2(
  .x = mappings,
  .y = names(x = mappings),
  .f = function(mapping, mapping_name)
  {
    if (grepl(pattern = "\\.csv", x = mapping)) {
      if (!file.exists(mapping)) {
        stop(paste(
          "The .csv provided for:",
          dQuote(mapping_name),
          "does not exist."
        ))
      }
      if (identical(x = mapping_name, y = "endowment_mapping")) {
        map <- .set_ele_read(file = mapping,
                             col = 2,
                             set_name = endowment_mapping)
      } else if (identical(x = mapping_name, y = "region_mapping")) {
        map <- .set_ele_read(file = mapping,
                             col = 2,
                             set_name = region_mapping)
      } else if (identical(x = mapping_name, y = "sector_mapping")) {
        map <- .set_ele_read(file = mapping,
                             col = 2,
                             set_name = sector_mapping)
      }
    } else {
      if (identical(x = mapping_name, y = "endowment_mapping")) {
        .internal_map_check(set = endowment_set,
                            database_version = database_version,
                            mapping = mapping)
      } else if (identical(x = mapping_name, y = "region_mapping")) {
        .internal_map_check(set = region_set,
                            database_version = database_version,
                            mapping = mapping)
      } else if (identical(x = mapping_name, y = "sector_mapping")) {
        .internal_map_check(set = tradables_set,
                            database_version = database_version,
                            mapping = mapping)
      }
    }
  }
)
if (verbose) {
with(data = ls_set_ele, expr = {
  .set_message(set_ele = endowment_mapping,
               set_name = "endowment",
               model_set = endowment_set)
  .set_message(set_ele = region_mapping,
               set_name = "region",
               model_set = region_set)
  .set_message(set_ele = sector_mapping,
               set_name = "sector",
               model_set = tradables_set)
})
}
config <- args_list
config
}
