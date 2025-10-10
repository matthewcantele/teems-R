#' Load and prepare data inputs
#'
#' @description `ems_data()` loads and prepares data for use
#'   within a CGE model run as well as converts GTAP databases
#'   between v6.2 and v7.0 formats.
#' 
#' @param dat_input Character of length 1, file name in working
#'   directory or path to a GTAP data "dat" HAR file. Input
#'   containing basedata coefficient data.
#' @param par_input Character of length 1, file name in working
#'   directory or path to a GTAP parameter "par" HAR file. Input
#'   containing parameter coefficient data.
#' @param set_input Character of length 1, file name in working
#'   directory or path to a GTAP set "set" HAR file. Input
#'   containing set elements and attributes.
#' @param time_steps Integer vector of variable length with
#'   initial value 0 or reference year corresponding to database
#'   employed (default is `NULL`). Input format can be either the
#'   chronological years of steps including initial reference
#'   year or steps from t0. For example, `c(2017, 2018, 2020,
#'   2022)` is identical to `c(c(0, 1, 3, 5))`.
#' @param aux_input Under development. Character of length 1,
#'   file name in working directory or path to a GTAP HAR file.
#'   Input containing auxillary coefficient data. Coefficients
#'   will be sorted into parameter and non-parameter lists.
#' @param convert_format Logical of length 1 (default is
#' `FALSE`). If `TRUE`, bidirectional conversion of data between
#' GTAP v6.2 and v7.0 formats will be carried out.
#' @param ... A named pairlist assigning either externally
#'   provided mappings (path to CSV) or internally available
#'   mappings to model sets that are read in (not constructed via
#'   any set operation defined in the model file). See
#'   \href{https://github.com/teems-org/teems-mappings/}{teems-mappings}
#'   for internally available mappings and formatting for
#'   user-provided mappings.
#'
#' @return A list of data.tables ready for input into the
#'   `"data"` argument of [`ems_deploy()`]
#' 
#' @seealso [`ems_deploy()`] for using the output of this function.
#' 
#' @examples
#' \dontrun{
#' # Static models (no data conversion)
#' v62_data <- ems_data(dat_input = "path/to/v62_data/v10A/flexagg10AY14/gsddat.har",
#'                      par_input = "path/to/v62_data/v10A/flexagg10AY14/gsdpar.har",
#'                      set_input = "path/to/v62_data/v10A/flexagg10AY14/gsdset.har",
#'                      REG = "big3",
#'                      TRAD_COMM = "macro_sector",
#'                      ENDW_COMM = "labor_agg")
#'
#' v70_data <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                      par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                      set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                      REG = "AR5",
#'                      COMM = "agriculture",
#'                      ACTS = "agriculture",
#'                      ENDW = "labor_diff")
#'                      
#' # Data format conversion
#' v62_data <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                      par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                      set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                      REG = "WB7",
#'                      TRAD_COMM = "manufacturing",
#'                      ENDW_COMM = "full",
#'                      convert_format = TRUE)
#'
#' # Intertemporal model (explicit time steps)
#' data <- ems_data(dat_input = "path/to/v62_data/v10A/flexagg10AY14/gsddat.har",
#'                  par_input = "path/to/v62_data/v10A/flexagg10AY14/gsdpar.har",
#'                  set_input = "path/to/v62_data/v10A/flexagg10AY14/gsdset.har",
#'                  REG = "WB23",
#'                  TRAD_COMM = "services",
#'                  ENDW_COMM = "labor_agg",
#'                  time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16))
#'  
#' # Intertemporal model (chronological time steps)
#' data <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                  par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                  set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                  REG = "R32",
#'                  COMM = "medium",
#'                  ACTS = "medium",
#'                  ENDW = "labor_diff",
#'                  time_steps = c(2017, 2018, 2020, 2022, 2024, 2026, 2028, 2030))                       
#' }           
#' @export
ems_data <- function(dat_input,
                     par_input,
                     set_input,
                     time_steps = NULL,
                     aux_input = NULL,
                     convert_format = FALSE,
                     ...) {
  if (missing(dat_input)) {
    .cli_missing(dat_input)
  }
  if (missing(par_input)) {
    .cli_missing(par_input)
  }
  if (missing(set_input)) {
    .cli_missing(set_input)
  }

  args_list <- mget(x = names(x = formals()))
  args_list$set_mappings <- list(...)
  args_list$... <- NULL
  call <- match.call()
  v <- .validate_data_args(
    a = args_list,
    call = call
  )

  i_data <- .load_input_data(
    dat_input = v$dat_input,
    par_input = v$par_input,
    set_input = v$set_input,
    aux_input = v$aux_input,
    call = call
  )

  if (convert_format) {
    i_data <- .convert_GTAPdb(i_data = i_data)
  }
  
  set_mappings <- .load_mappings(
    set_mappings = v$set_mappings,
    time_steps = time_steps,
    metadata = attr(i_data, "metadata"),
    call = call
  )

  # if (!is.null(args_list$aux_input)) {
  #   ls_data <- .distribute_aux(tab_file = args_list$tab_file,
  #                              ls_aux = args_list$aux,
  #                              ls_data = ls_data)
  # }

  i_data <- .process_data(
    i_data = i_data,
    set_mappings = set_mappings,
    call = call
  )

  i_data
}