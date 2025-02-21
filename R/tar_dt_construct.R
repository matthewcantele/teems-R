#' @importFrom data.table as.data.table setnames setDT
#' 
#' @keywords internal
#' @noRd
.construct_dt <- function(ls_array,
                          metadata,
                          data_type,
                          coeff_extract,
                          sets = NULL) {

  # create list of data.tables from arrays
  if (!is.null(x = sets)) {
  int_sets <- subset(x = sets,
                     subset = intertemporal,
                     select = name)[[1]]
  }

  ls_array <- lapply(X = ls_array, FUN = function(header) {
    dim_length <- length(x = dimnames(x = header[["data"]]))
    # set file
    if (identical(x = dim_length, y = 0L)) {
      header[["dt"]] <- data.table::as.data.table(x = as.matrix(x = header[["data"]]))
      if (!identical(x = data_type, y = "set")) {
        data.table::setnames(x = header[["dt"]], new = "Value")
      } else if (identical(x = data_type, y = "set")) {
        data.table::setnames(x = header[["dt"]], new = header[["header_name"]])
      }
    } else {
      header[["dt"]] <- array2DF(x = header[["data"]])
      stnd_col <- substr(x = colnames(header[["dt"]]), start = 1, stop = nchar(colnames(header[["dt"]])) - 1)
      if (any(is.element(el = stnd_col, set = int_sets))) {
        int_col <- colnames(header[["dt"]])[is.element(el = stnd_col, set = int_sets)]
        header[["dt"]][[int_col]] <- as.integer(x = header[["dt"]][[int_col]])
      }
      data.table::setDT(x = header[["dt"]])
    } 
    return(header)
  })

  attr(x = ls_array, which = "data_type") <- data_type
  return(ls_array)
}
