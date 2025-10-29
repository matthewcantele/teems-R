#' @keywords internal
#' @noRd
.custom_mod <- function(arr,
                        ...) {
  UseMethod(".custom_mod")
}

#' @method .custom_mod default
#' @keywords internal
#' @noRd
#' @export
.custom_mod.default <- function(arr,
                                ...) {
  return(arr)
}

#' @importFrom data.table setnames
#' @method .custom_mod ETRE
#' @keywords internal
#' @noRd
#' @export
.custom_mod.ETRE <- function(arr,
                             i_data) {
  if (.o_expand_ETRE()) {
    if (attr(i_data, "metadata")$data_format %=% "v6.2") {
      endowments <- i_data$H6
    } else if (attr(i_data, "metadata")$data_format %=% "v7.0") {
      endowments <- i_data$ENDW
    }
    
    endw_name <- class(endowments)[2]
    dim_index <- grep("ENDW", names(dimnames(arr)))
    current_names <- dimnames(arr)[[dim_index]]
    missing_entries <- setdiff(endowments, current_names)

    if (length(missing_entries) %=% 0L) {
      return(arr)
    }

    classes <- class(arr)
    new_dims <- dim(arr)
    new_dims[dim_index] <- length(missing_entries)
    new_slice_dimnames <- dimnames(arr)
    new_slice_dimnames[[dim_index]] <- missing_entries
    new_slice <- array(-1e-05, new_dims, new_slice_dimnames)
    arr <- .abind(arr, new_slice, along = dim_index)
    names(dimnames(arr)) <- names(new_slice_dimnames)
    names(dimnames(arr))[dim_index] <- endw_name
    class(arr) <- unique(c(classes, class(arr)))
  }
  return(arr)
}