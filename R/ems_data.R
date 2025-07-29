#' `r lifecycle::badge("experimental")` Modify data inputs.
#'
#' @description `ems_data()` prepares data for various
#'   non-standard models including intertemporal runs, database
#'   format conversion as well as converts to human-readable
#'   format.
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
#' @param time_steps Integer vector of variable length (default
#'   is `NULL`). `"time_steps"` are inputted as the chronological
#'   years of steps including initial reference year or steps
#'   from t0. For example, `c(2017, 2020, 2024, 2028, 2032, 2038,
#'   2044, 2050)` is identical to `c(0, 3, 7, 11, 15, 21, 27,
#'   33)`.
#' @export
ems_data <- function(dat_input,
                     par_input,
                     set_input,
                     aux_input = NULL,
                     tab_file = NULL,
                     target_format = NULL,
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
  if (!missing(...)) {
    aux_headers <- list(...)
  }
  args_list <- mget(x = names(x = formals()))
  args_list$... <- NULL
  call <- match.call()
  args_list <- .validate_data_args(
    args_list = args_list,
    call = call
  )

  coeff_extract <- .process_tablo(
    tab_file = args_list$tab_file,
    type = "coefficient",
    call = call
  )

  set_extract <- .process_tablo(
    tab_file = args_list$tab_file,
    type = "set",
    call = call
  )$sets
  
  ls_data <- list(
    dat = args_list$dat,
    par = args_list$par,
    set = args_list$set
  )
  
  if (!is.null(args_list$aux_input)) {
    ls_data <- .distribute_aux(tab_file = args_list$tab_file,
                               ls_aux = args_list$aux,
                               ls_data = ls_data)
  }

  if (!is.null(args_list$target_format)) {
    ls_data <- .convert_db(
      tab_file = args_list$tab_file,
      ls_data = ls_data,
      set_extract = set_extract,
      coeff_extract = coeff_extract,
      metadata = args_list$metadata,
      target_format = args_list$target_format
    )
  } else {
    attr(ls_data, "metadata") <- args_list$metadata
  }

  if (!is.null(args_list$time_steps)) {
    ls_data <- .inject_time(
      ls_data = ls_data,
      time_steps = args_list$time_steps,
      tab_file = args_list$tab_file,
      set_extract = set_extract,
      coeff_extract = coeff_extract,
      call
    )
  }

  metadata <- attr(ls_data, "metadata")
  data_id <- uuid::UUIDgenerate()
  ls_data <- purrr::map2(
    ls_data,
    names(ls_data),
    function(d, n) {
      structure(d,
        data_type = n,
        data_id = data_id,
        metadata = metadata
      )
    }
  )

  ls_data
}