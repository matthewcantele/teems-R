#' @keywords internal
#' @noRd
.header2v7 <- function(input,
                       ...) {
  UseMethod(".header2v7")
}

#' @method .header2v7 default
#' @keywords internal
#' @noRd
#' @export
.header2v7.default <- function(input,
                               ...) {
  browser()
}

#' @method .header2v7 dat
#' @keywords internal
#' @noRd
#' @export
.header2v7.dat <- function(input,
                           ...) {
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 par
#' @keywords internal
#' @noRd
#' @export
.header2v7.par <- function(input,
                           ...) {
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 set
#' @keywords internal
#' @noRd
#' @export
.header2v7.set <- function(input,
                           ...) {
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 VDFM
#' @keywords internal
#' @noRd
#' @export
.header2v7.VDFM <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2v7 VDFA
#' @keywords internal
#' @noRd
#' @export
.header2v7.VDFA <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2v7 VIFM
#' @keywords internal
#' @noRd
#' @export
.header2v7.VIFM <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2v7 VIFA
#' @keywords internal
#' @noRd
#' @export
.header2v7.VIFA <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2v7 ISEP
#' @keywords internal
#' @noRd
#' @export
.header2v7.ISEP <- function(input,
                            ...) {

  classes <- class(input)
  input <- input[, length(dimnames(input)$PROD_COMM), , ]
  input <- input * -1
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 EVFA
#' @keywords internal
#' @noRd
#' @export
.header2v7.EVFA <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 VFM
#' @keywords internal
#' @noRd
#' @export
.header2v7.VFM <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 FBEP
#' @keywords internal
#' @noRd
#' @export
.header2v7.FBEP <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 FTRV
#' @keywords internal
#' @noRd
#' @export
.header2v7.FTRV <- function(input,
                            ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 EVOA
#' @keywords internal
#' @noRd
#' @export
.header2v7.EVOA <- function(input,
                            i_data) {

  classes <- class(input)
  VFM_arr <- i_data$VFM
  VFM_arr <- VFM_arr[, -length(dimnames(VFM_arr)[[2]]), ]
  VFM_arr_dimnames <- dimnames(VFM_arr)
  VFM_arr_total <- apply(VFM_arr, c(1, 3), sum)
  
  phi_arr <- array( 0, dim(VFM_arr), dimnames(VFM_arr))
  new_arr <- array(0, dim(VFM_arr), dimnames(VFM_arr))
  
  # calculate percentage along REG,ENDW and use share with EVOA
  for (i in 1:length(VFM_arr_dimnames$ENDW_COMM)) {
    for (k in 1:length(VFM_arr_dimnames$REG)) {
      phi_arr[i, , k] <- VFM_arr[i, , k] / VFM_arr_total[i, k]
      new_arr[i, , k] <- phi_arr[i, , k] * input[i, k]
    }
  }

  input <- new_arr
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 ESBD
#' @keywords internal
#' @noRd
#' @export
.header2v7.ESBD <- function(input,
                            i_data) {
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 ESBM
#' @keywords internal
#' @noRd
#' @export
.header2v7.ESBM <- function(input,
                            i_data) {
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 ETRE
#' @keywords internal
#' @noRd
#' @export
.header2v7.ETRE <- function(input,
                            i_data) {
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 ESBT
#' @keywords internal
#' @noRd
#' @export
.header2v7.ESBT <- function(input,
                            i_data) {
  input <- .drop_CGDS(input = input)
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2v7 ESBV
#' @keywords internal
#' @noRd
#' @export
.header2v7.ESBV <- function(input,
                            i_data) {
  input <- .drop_CGDS(input = input)
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}