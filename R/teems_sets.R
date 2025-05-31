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
#' @param time_steps Integer vector of variable length (default is `NULL`).
#'   `"time_steps"` are inputted as the desired chronological years of steps
#'   including initial reference year.
#' @param interval_switch Logical length 1 (default is `FALSE`). Switch
#'   controlling interpretation of `"time_steps"` input. When `TRUE`, each
#'   element of `"time_steps"` is interpreted as an interval between 2 time
#'   steps.
#' @importFrom rlang current_env
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
#' unlink(x = temp_dir, recursive = TRUE)
#'
#' @export
teems_sets <- function(set_input,
                       aux_set = NULL,
                       ...,
                       model_version = NULL,
                       quiet = FALSE)
{
call <- match.call()
args_list <- mget(x = names(x = formals()))
args_list[["set_input"]] <- .check_input(file = set_input,
                                         valid_ext = c("har", "qs2"),
                                         call = call,
                                         internal = FALSE)
metadata <- .get_metadata(con = args_list[["set_input"]])
if (!is.null(x = aux_set)) {
  aux_set_file <- .teems_cache(input = aux_set,
                               file = "aux_set",
                               ext = "qs2",
                               dir = "inputdata")
  args_list[["aux_set_file"]] <- aux_set_file
}
sets <- .check_set_mappings(set_mappings = list(...),
                            time_format = time_format,
                            metadata = metadata,
                            call = call,
                            envir = rlang::current_env(),
                            quiet = quiet)
args_list[["..."]] <- NULL
config <- c(args_list,
            sets,
            metadata = metadata,
            call = call)
config[["quiet"]] <- NULL
config
}
