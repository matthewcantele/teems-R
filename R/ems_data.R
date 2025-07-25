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
  )
  
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
    browser()
    ls_data <- .convert_db(
      tab_file = args_list$tab_file,
      ls_dat = args_list$dat,
      ls_par = args_list$par,
      ls_set = args_list$set,
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