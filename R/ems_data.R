#' Modify data inputs
#'
#' @description `ems_data()` prepares data for various
#'   non-standard models including intertemporal runs as well as
#'   converts GTAP databases between v6.2 and v7.0 formats.
#'
#' @inheritParams ems_model
#' @param dat_input Character of length 1, file name in working
#'   directory or path to a GTAP data "dat" HAR file. Input
#'   containing base coefficient data.
#' @param par_input Character of length 1, file name in working
#'   directory or path to a GTAP parameter "par" HAR file. Input
#'   containing parameter coefficient data.
#' @param aux_input Character of length 1, file name in working
#'   directory or path to a GTAP HAR file. Input containing
#'   auxillary coefficient data. Coefficients will be sorted into
#'   parameter and non-parameter lists.
#' @param target_format Character of length 1, default `NULL`. In
#'   the case of database conversion, either "v6.2" or "v7.0" as
#'   the desired format.
#' @param time_steps Integer vector of variable length with
#'   initial value 0 or reference year corresponding to database
#'   employed (default is `NULL`). Input format can be either the
#'   chronological years of steps including initial reference
#'   year or steps from t0. For example, `c(2017, 2018, 2020,
#'   2022)` is identical to `c(c(0, 1, 3, 5))`.
#' @param ... Future extension
#'
#' @return A list ready for input into the `"data"` argument
#'   of [`ems_deploy()`]
#' 
#' @seealso [`ems_deploy()`] for using the output of this function.
#' 
#' @examples
#' \dontrun{
#' # Data format conversions
#' v62_data <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                      par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                      set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                      tab_file = "GTAPv6.2",
#'                      target_format = "v6.2")
#'
#' v7_data <- ems_data(dat_input = "path/to/v62_data/v10A/flexagg10AY14/gsddat.har",
#'                     par_input = "path/to/v62_data/v10A/flexagg10AY14/gsdpar.har",
#'                     set_input = "path/to/v62_data/v10A/flexagg10AY14/gsdset.har",
#'                     tab_file = "GTAPv7.0",
#'                     target_format = "v7.0")
#' 
#' # Intertemporal modification (GTAP-REv1)
#' GTAP_RE <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                     par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                     set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                     tab_file = "GTAP-REv1",
#'                     time_steps = c(2017, 2018, 2019, 2020))
#'  
#' # Intertemporal modification with data format conversion (GTAP-INTv1)
#' GTAP_INT <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                      par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                      set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                      tab_file = "GTAP-INTv1",
#'                      target_format = "v6.2",
#'                      time_steps = c(0, 1, 2, 3))                          
#' }           
#' @export
ems_data <- function(dat_input,
                     par_input,
                     set_input,
                     aux_input = NULL,
                     unaggregated_input = NULL,
                     aggregated_input = NULL,
                     convert_format = FALSE,
                     time_steps = NULL,
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
  
  i_data <- .load_input_data(dat_input = v$dat_input,
                             par_input = v$par_input,
                             set_input = v$set_input,
                             aux_input = v$aux_input,
                             call = call)

  if (convert_format) {
      i_data <- .convert_GTAPdb(i_data = i_data)
  }
  set_mappings <- .load_mappings(set_mappings = v$set_mappings,
                                 time_steps = time_steps,
                                 metadata = attr(i_data, "metadata"),
                                 call = call)
  # coeff_extract <- .process_tablo(
  #   tab_file = args_list$tab_file,
  #   type = "coefficient",
  #   call = call
  # )
  # 
  # set_extract <- .process_tablo(
  #   tab_file = args_list$tab_file,
  #   type = "set",
  #   call = call
  # )$sets
  
  # ls_data <- list(
  #   dat = args_list$dat,
  #   par = args_list$par,
  #   set = args_list$set
  # )
  
  # if (!is.null(args_list$aux_input)) {
  #   ls_data <- .distribute_aux(tab_file = args_list$tab_file,
  #                              ls_aux = args_list$aux,
  #                              ls_data = ls_data)
  # }
  # 
  # if (!is.null(args_list$target_format)) {
  # if (purrr::pluck(args_list, "metadata", "data_format") %=% args_list$target_format) {
  #   .cli_action(
  #     data_err$invalid_convert,
  #     action = "abort",
  #     call = call
  #   )
  # }
  #   ls_data <- .convert_db(
  #     tab_file = args_list$tab_file,
  #     ls_data = ls_data,
  #     set_extract = set_extract,
  #     coeff_extract = coeff_extract,
  #     metadata = args_list$metadata,
  #     target_format = args_list$target_format
  #   )
  # } else {
  #   attr(ls_data, "metadata") <- args_list$metadata
  # }
  # 
  # if (!is.null(args_list$time_steps)) {

  #   ls_data <- .inject_time(
  #     ls_data = ls_data,
  #     time_steps = args_list$time_steps,
  #     tab_file = args_list$tab_file,
  #     set_extract = set_extract,
  #     coeff_extract = coeff_extract,
  #     metadata = args_list$metadata,
  #     call
  #   )
  # }

  i_data <- .process_data(i_data = i_data,
                          set_mappings = set_mappings,
                          call = call)

  i_data
}