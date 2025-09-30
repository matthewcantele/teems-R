#' @keywords internal
#' @noRd
.header2v6 <- function(input,
                       ...) {
  UseMethod(".header2v6")
}

#' @method .header2v6 default
#' @keywords internal
#' @noRd
#' @export
.header2v6.default <- function(input,
                               i_data) {
  NextMethod()
}

#' @method .header2v6 dat
#' @keywords internal
#' @noRd
#' @export
.header2v6.dat <- function(input,
                           i_data) {
  
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 par
#' @keywords internal
#' @noRd
#' @export
.header2v6.par <- function(input,
                           i_data) {
  v6col <- unlist(subset(
    coeff_conversion,
    v7.0header %in% class(input)[1],
    v6.2set
  ))

  classes <- class(input)
  
  if (!anyNA(v6col) && !is.null(v6col)) {
    orig_dim_names <- names(dimnames(input))
    r_idx <- match(orig_dim_names, set_conversion$v7.0name)
    cnvrt_dim_names <- ifelse(is.na(r_idx),
      orig_dim_names,
      set_conversion$v6.2name[r_idx]
    )

    drop_dim <- cnvrt_dim_names[!cnvrt_dim_names %in% v6col]

    if (!drop_dim %=% character(0)) {
      input <- apply(input,
        which(!orig_dim_names %in% drop_dim),
        FUN = unique
      )
    }

    if (classes[1] %=% "ESBT") {
      CGDS <- array(0, dimnames = list("CGDS"))
      input <- c(input, CGDS)
    } else if (classes[1] %=% "ESBV") {
      CGDS <- array(1, dimnames = list("CGDS"))
      input <- c(input, CGDS)
    }

    if (!is.array(input)) {
      input <- as.array(input)
    }

    names(dimnames(input)) <- v6col
  }
  
  class(input) <- unique(c(classes, class(input)))
  class(input)[3] <- "v6.2"
  return(input)
}

#' @method .header2v6 set
#' @keywords internal
#' @noRd
#' @export
.header2v6.set <- function(input,
                           i_data) {
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 VDFB
#' @keywords internal
#' @noRd
#' @export
.header2v6.VDFB <- function(input,
                            i_data) {

  classes <- class(input)
  input <- .convert_invest_v(input = input,
                             CGDS_data = i_data[["VDIB"]])
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 VDFP
#' @keywords internal
#' @noRd
#' @export
.header2v6.VDFP <- function(input,
                            i_data) {
  classes <- class(input)
  input <- .convert_invest_v(input = input,
                             CGDS_data = i_data[["VDIP"]])
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 VMFB
#' @keywords internal
#' @noRd
#' @export
.header2v6.VMFB <- function(input,
                            i_data) {
  classes <- class(input)
  input <- .convert_invest_v(input = input,
                             CGDS_data = i_data[["VMIB"]])
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 VMFP
#' @keywords internal
#' @noRd
#' @export
.header2v6.VMFP <- function(input,
                            i_data) {
  classes <- class(input)
  input <- .convert_invest_v(input = input,
                             CGDS_data = i_data[["VMIP"]])
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 EVFB
#' @keywords internal
#' @noRd
#' @export
.header2v6.EVFB <- function(input,
                            ...) {
  classes <- class(input)
  input <- .convert_invest_e(input = input)
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 EVFP
#' @keywords internal
#' @noRd
#' @export
.header2v6.EVFP <- function(input,
                            ...) {
  classes <- class(input)
  input <- .convert_invest_e(input = input)
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 FBEP
#' @keywords internal
#' @noRd
#' @export
.header2v6.FBEP <- function(input,
                            ...) {
  classes <- class(input)
  input <- .convert_invest_e(input = input)
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 FTRV
#' @keywords internal
#' @noRd
#' @export
.header2v6.FTRV <- function(input,
                            ...) {
  classes <- class(input)
  input <- .convert_invest_e(input = input)
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v6 EVOS
#' @keywords internal
#' @noRd
#' @export
.header2v6.EVOS <- function(input,
                            ...) {
  classes <- class(input)
  arr_names <- names(dimnames(input))
  sum_dim <- "ACTS"
  xACTS_dim <- which(!arr_names %in% sum_dim)
  input <- apply(input, xACTS_dim, sum)
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @importFrom abind abind
#' @method .header2v6 ISEP
#' @keywords internal
#' @noRd
#' @export
.header2v6.ISEP <- function(input,
                            i_data) {
  classes <- class(input)
  arr_dimnames <- dimnames(input)
  arr_names <- names(arr_dimnames)
  CGDS_dim <- c(dim(input), 1)
  CGDS_dimnames <- c(arr_dimnames, list(ACTS = "CGDS"))
  CGDS_data <- array(input, CGDS_dim, CGDS_dimnames)
  CGDS_data <- CGDS_data * -1
  arr <- i_data$CSEP
  arr <- arr * -1
  arr_names <- names(dimnames(arr))

  a_idx <- match(arr_names, names(CGDS_dimnames))
  CGDS_data <- aperm(CGDS_data, a_idx)
  ACTS_dim <- which(arr_names %in% "ACTS")
  input <- abind::abind(arr, CGDS_data, along = ACTS_dim)
  names(dimnames(input)) <- arr_names
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}