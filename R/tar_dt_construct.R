#' @importFrom data.table as.data.table setnames setDT
#' @importFrom purrr pluck
#' @return A list of data.tables for each header and metadata file for each data
#'   type.
#' @keywords internal
#' @noRd
.construct_dt <- function(ls_array,
                          metadata,
                          coeff_extract,
                          set_array = NULL) {
  # create list of data.tables from arrays
  # uppercase 'Value' used to distinguish from automatically generated columns
  data_type <- attr(x = ls_array, "data_type")
  ls_array <- lapply(X = ls_array, FUN = function(header) {
    dim_length <- length(x = dimnames(x = header[["data"]]))
    # set file
    if (identical(x = dim_length, y = 0L)) {
      header[["dt"]] <- data.table::as.data.table(x = header[["data"]])
      if (!identical(x = data_type, y = "set")) {
        data.table::setnames(x = header[["dt"]], new = "Value")
      } else if (identical(x = data_type, y = "set")) {
        data.table::setnames(x = header[["dt"]], new = header[["header_name"]])
      }
    } else {
            header[["dt"]] <- array2DF(x = header[["data"]])
      data.table::setDT(x = header[["dt"]])
    } 
    return(header)
  })

  attr(x = ls_array, which = "data_type") <- data_type
  return(ls_array)
}
