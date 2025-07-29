#' @keywords internal
#' @noRd
.convert_db <- function(tab_file,
                        ls_data,
                        set_extract,
                        coeff_extract,
                        metadata,
                        target_format,
                        call) {

  ls_data$dat <- .convert_format(
    input = ls_data$dat,
    target_format = target_format,
    coeff_extract = coeff_extract
  )

  ls_data$par <- .convert_format(
    input = ls_data$par,
    target_format = target_format,
    coeff_extract = coeff_extract
  )

  ls_data$set <- .convert_format(
    input = ls_data$set,
    set_extract = set_extract,
    target_format = target_format
  )

  metadata$data_format <- target_format
  attr(ls_data, "metadata") <- metadata

  return(ls_data)
}