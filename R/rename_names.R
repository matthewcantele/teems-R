#' @keywords internal
#' @noRd
.names_rename <- function(input) {
  UseMethod(".names_rename")
}

#' @method .names_rename default
#' @keywords internal
#' @noRd
#' @export
.names_rename.default <- function(input) {
  origin <- class(input)[3]
  target <- ifelse(inherits(input, "v7.0"), "v6.2", "v7.0")
  id <- match(class(input)[1], coeff_conversion[[paste0(origin, "header")]])


  if (!is.na(id)) {
    new_header_name <- coeff_conversion[id, paste0(target, "header")]
    class(input)[1] <- new_header_name
  }

  if (is.numeric(input)) {
    nme_dimname <- names(dimnames(input))
    if (!is.null(nme_dimname)) {
      r_idx <- match(
        nme_dimname,
        with(set_conversion, get(paste0(origin, "name")))
      )

      new_dimnames <- with(
        set_conversion,
        get(paste0(target, "name"))[r_idx]
      )

      if (any(is.na(new_dimnames))) {
        revert <- which(is.na(new_dimnames))
        new_dimnames[revert] <- nme_dimname[revert]
      }
      names(dimnames(input)) <- new_dimnames
    }
  }

  class(input)[3] <- target
  return(input)
}

#' @method .names_rename set
#' @keywords internal
#' @noRd
.names_rename.set <- function(input) {
  origin <- class(input)[4]
  target <- ifelse(inherits(input, "v7.0"), "v6.2", "v7.0")

  id <- match(class(input)[1], set_conversion[[paste0(origin, "header")]])
  if (!is.na(id)) {
    new_header_name <- set_conversion[id, paste0(target, "header")]
    new_set_name <- set_conversion[id, paste0(target, "name")]
    class(input)[1] <- new_header_name
    class(input)[2] <- new_set_name
  }
  class(input)[4] <- target
  return(input)
}




