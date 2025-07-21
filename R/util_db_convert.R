#' @keywords internal
#' @noRd
.convert_db <- function(tab_file,
                        ls_dat,
                        ls_par,
                        ls_set,
                        metadata,
                        target_format,
                        call) {
  set_extract <- .process_tablo(
    tab_file = tab_file,
    type = "set",
    call = call
  )

  coeff_extract <- .process_tablo(
    tab_file = tab_file,
    type = "coefficient",
    call = call
  )

  ls_dat <- .convert_format(
    input = ls_dat,
    target_format = target_format,
    coeff_extract = coeff_extract
  )

  ls_par <- .convert_format(
    input = ls_par,
    target_format = target_format,
    coeff_extract = coeff_extract
  )

  ls_set <- .convert_format(
    input = ls_set,
    set_extract = set_extract$sets,
    target_format = target_format
  )

  cvrted_data <- list(
    dat = ls_dat,
    par = ls_par,
    set = ls_set
  )

  metadata$data_format <- target_format
  attr(cvrted_data, "metadata") <- metadata

  return(cvrted_data)
}