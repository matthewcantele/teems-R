#' @note This function is a stripped down version of abind::abind
#'   and has been adopted and modified as such to avoid importing
#'   the abind package for a single function.
#'
#' @keywords internal
#' @noRd
.abind <- function(..., along = N) {
  arg.list <- list(...)
  N <- max(1, sapply(list(...), function(x) length(dim(x))))
  have.list.arg <- FALSE

  pre <- seq(from = 1, length.out = along - 1)
  post <- seq(to = N - 1, length.out = N - along)
  perm <- c(seq(length.out = N)[-along], along)
  arg.names <- names(arg.list)
  arg.names <- rep("", length(arg.list))
  arg.alt.names <- arg.names
  names(arg.list) <- arg.names
  arg.dimnames <- matrix(vector("list", N * length(arg.names)),
    nrow = N, ncol = length(arg.names)
  )
  dimnames(arg.dimnames) <- list(NULL, arg.names)
  arg.dnns <- matrix(vector("list", N * length(arg.names)),
    nrow = N, ncol = length(arg.names)
  )
  dimnames(arg.dnns) <- list(NULL, arg.names)
  dimnames.new <- vector("list", N)
  arg.dim <- matrix(integer(1), nrow = N, ncol = length(arg.names))
  for (i in seq(length.out = length(arg.list))) {
    m <- arg.list[[i]]
    m.changed <- FALSE
    if (is.data.frame(m)) {
      m <- as.matrix(m)
      m.changed <- TRUE
    } else if (!is.array(m) && !is.null(m)) {
      if (!is.atomic(m)) {
        stop("arg '", arg.alt.names[i], "' is non-atomic")
      }
      dn <- names(m)
      m <- as.array(m)
      if (length(dim(m)) == 1 && !is.null(dn)) {
        dimnames(m) <- list(dn)
      }
      m.changed <- TRUE
    }
    new.dim <- dim(m)
    if (length(new.dim) == N) {
      if (!is.null(dimnames(m))) {
        arg.dimnames[, i] <- dimnames(m)
      }
      arg.dim[, i] <- new.dim
    } else if (length(new.dim) == N - 1) {
      if (!is.null(dimnames(m))) {
        arg.dimnames[-along, i] <- dimnames(m)
        dimnames(m) <- NULL
      }
      arg.dim[, i] <- c(new.dim[pre], 1, new.dim[post])
      if (any(perm != seq(along.with = perm))) {
        dim(m) <- c(new.dim[pre], 1, new.dim[post])
        m.changed <- TRUE
      }
    } else {
      stop(
        "'", arg.alt.names[i], "' does not fit: should have `length(dim())'=",
        N, " or ", N - 1
      )
    }
    if (any(perm != seq(along.with = perm))) {
      arg.list[[i]] <- aperm(m, perm)
    } else if (m.changed) {
      arg.list[[i]] <- m
    }
  }

  conform.dim <- arg.dim[, 1]
  for (i in seq(length.out = ncol(arg.dim))) {
    if (any((conform.dim != arg.dim[, i])[-along])) {
      stop("arg '", arg.alt.names[i], "' has dims=", paste(arg.dim[
        ,
        i
      ], collapse = ", "), "; but need dims=", paste(replace(
        conform.dim,
        along, "X"
      ), collapse = ", "))
    }
  }
  if (N > 1) {
    for (dd in seq(length.out = N)[-along]) {
      for (i in rev(seq(along.with = arg.names))) {
        if (length(arg.dimnames[[dd, i]]) > 0) {
          dimnames.new[[dd]] <- arg.dimnames[[dd, i]]
          break
        }
      }
    }
  }

  for (i in seq(length.out = length(arg.names))) {
    if (arg.dim[along, i] > 0) {
      dnm.along <- arg.dimnames[[along, i]]
      if (length(dnm.along) == arg.dim[along, i]) {
        use.along.names <- TRUE
      } else {
        if (arg.dim[along, i] == 1) {
          dnm.along <- arg.names[i]
        } else if (arg.names[i] == "") {
          dnm.along <- rep("", arg.dim[along, i])
        } else {
          dnm.along <- paste(arg.names[i], seq(length.out = arg.dim[
            along,
            i
          ]), sep = "")
        }
      }
      dimnames.new[[along]] <- c(
        dimnames.new[[along]],
        dnm.along
      )
    }
  }
  if (!use.along.names) {
    dimnames.new[along] <- list(NULL)
  }
  out <- array(unlist(arg.list, use.names = FALSE), dim = c(arg.dim[
    -along,
    1
  ], sum(arg.dim[along, ])), dimnames = dimnames.new[perm])
  if (any(order(perm) != seq(along.with = perm))) {
    out <- aperm(out, order(perm))
  }

  return(out)
}