#' @keywords internal
#' @noRd
.v7_new_header <- function(input,
                           ...) {
  UseMethod(".v7_new_header")
}

#' @method .v7_new_header default
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.default <- function(input,
                                   ...) {
  return(NULL)
}

#' @method .v7_new_header VDFM
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.VDFM <- function(input,
                                ...) {
  input <- .convert_invest_v(
    input = input,
    new_h = "VDIB"
  )
  input <- .names_rename(input = input)
  return(input)
}

#' @method .v7_new_header VDFA
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.VDFA <- function(input,
                                ...) {
  input <- .convert_invest_v(
    input = input,
    new_h = "VDIP"
  )
  input <- .names_rename(input = input)
  return(input)
}

#' @method .v7_new_header VIFM
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.VIFM <- function(input,
                                ...) {
  input <- .convert_invest_v(
    input = input,
    new_h = "VMIB"
  )
  input <- .names_rename(input = input)
  return(input)
}

#' @method .v7_new_header VIFA
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.VIFA <- function(input,
                                ...) {
  input <- .convert_invest_v(
    input = input,
    new_h = "VMIP"
  )
  input <- .names_rename(input = input)
  return(input)
}

#' @method .v7_new_header ISEP
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.ISEP <- function(input,
                                ...) {
  PROD_dim <- which(names(dimnames(input)) %in% "PROD_COMM")
  input <- input[, -dim(input)[PROD_dim], , ]
  input <- input * -1
  class(input) <- c("CSEP", "dat", "v6.2", class(input))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .v7_new_header TVOM
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.TVOM <- function(input,
                                REG,
                                COMM,
                                ACTS,
                                OSEP,
                                ...) {
  MAKB <- array(
    0,
    c(length(COMM), length(COMM), length(REG)),
    list(COMM = COMM, ACTS = ACTS, REG = REG)
  )

  # diagonal make
  for (k in 1:length(REG)) {
    for (i in 1:length(COMM)) {
      MAKB[i, i, k] <- input[i, k]
    }
  }

  MAKS <- array(
    0,
    c(length(COMM), length(COMM), length(REG)),
    list(COMM = COMM, ACTS = ACTS, REG = REG)
  )

  for (k in 1:length(x = REG)) {
    for (i in 1:length(x = COMM)) {
      MAKS[i, i, k] <- OSEP[i, k]
    }
  }
  MAKS <- MAKB + MAKS

  class(MAKB) <- c("MAKB", "dat", "v7.0", class(MAKB))
  class(MAKS) <- c("MAKS", "dat", "v7.0", class(MAKS))
  return(list(MAKB, MAKS))
}

#' @method .v7_new_header ESBG
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.ESBG <- function(input,
                                REG,
                                ...) {
  input <- .create_v7arr(
    input = input,
    value = 1,
    REG = REG
  )
  return(input)
}

#' @method .v7_new_header ETRQ
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.ETRQ <- function(input,
                                REG,
                                ACTS,
                                ...) {
  input <- .create_v7arr(
    input = input,
    value = -5,
    REG = REG,
    ACTS = ACTS
  )

  return(input)
}

#' @method .v7_new_header ESBS
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.ESBS <- function(input,
                                MARG,
                                ...) {
  input <- .create_v7arr(
    input = input,
    value = 1,
    MARG = MARG
  )
  return(input)
}

#' @method .v7_new_header ESBC
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.ESBC <- function(input,
                                REG,
                                ACTS,
                                ...) {
  input <- .create_v7arr(
    input = input,
    value = 0,
    REG = REG,
    ACTS = ACTS
  )

  return(input)
}

#' @method .v7_new_header ESBQ
#' @keywords internal
#' @noRd
#' @export
.v7_new_header.ESBQ <- function(input,
                                REG,
                                COMM,
                                ...) {
  input <- .create_v7arr(
    input = input,
    value = 0,
    REG = REG,
    COMM = COMM
  )
  return(input)
}