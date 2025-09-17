#' @importFrom data.table as.data.table setnames
#' 
#' @keywords internal
#' @noRd
.array2DT <- function(i_data) {
  i_data <- lapply(i_data, function(arr) {
    dim_length <- length(dimnames(arr))
    if (dim_length %=% 0L) {
      dt <- data.table::as.data.table(as.matrix(arr))
      data.table::setnames(dt, new = "Value")
    } else {
      dt <- array2DF(arr)
      dt <- data.table::as.data.table(dt)
    }

    if (inherits(arr, c("dat", "par"))) {
      class(dt) <- c(class(arr)[1:2], class(dt))
    } else if (inherits(arr, "set")) {
      class(dt) <- c(class(arr)[1:3], class(dt))
    }

    return(dt)
  })
  return(i_data)
}