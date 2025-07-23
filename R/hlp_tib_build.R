#' @importFrom tibble tibble
#' @importFrom data.table as.data.table setnames setDT setkey
#' @importFrom purrr list_rbind map discard map2
#' 
#' @keywords internal
#' @noRd
.build_tibble <- function(ls_,
                          sets,
                          ...) {
  UseMethod(".build_tibble")
}

#' @export
.build_tibble.coefficient <- function(ls_,
                                      sets,
                                      unaggregated_input = NULL,
                                      coeff_extract) {
  # could strip time sets to speed up dt creation then add
  int_sets <- subset(sets, intertemporal, name)[[1]]
  ls_ <- lapply(ls_, function(header) {
    dim_length <- length(dimnames(header$data))
    # set file
    if (dim_length %=% 0L) {
      header$dt <- data.table::as.data.table(as.matrix(header$data))
      data.table::setnames(header$dt, new = "Value")
    # } else if (dim_length %=% 1L) {
    #   header$dt <- data.table::as.data.table(array2DF(header$data),
    #                                          value.name = "Value")
    } else {
      header$dt <- array2DF(header$data)
      header$dt <- data.table::as.data.table(header$dt)
      col <- colnames(header$dt)
      int_col <- match(int_sets, .dock_tail(col))
      if (any(!is.na(int_col))) {
        int_col <- purrr::discard(int_col, is.na)
        header$dt[, (int_col) := lapply(.SD, as.integer), .SDcols = int_col]
      }
    }
    return(header)
  })

  if (!is.null(unaggregated_input)) {
    ls_ <- .inject_unagg_input(
      ls_data = ls_,
      unaggregated_input = unaggregated_input
    )
  }

  l_idx <- match(names(ls_), coeff_extract$header)
  ls_ <- purrr::map2(ls_, l_idx, function(h, id) {
    h$file <- coeff_extract$file[id]
    h$data_type <- coeff_extract$data_type[id]
    if (grepl("integer", coeff_extract$qualifier_list[id])) {
      h$type <- "integer"
    } else if (!is.na(id)) {
      h$type <- "real"
    } else {
      h$type <- NA
    }
    return(h)
  })

  tib_data <- purrr::list_rbind(purrr::map(ls_, function(x) {
    tibble::tibble(
      header = x$header,
      label = x$label,
      coefficient = x$coefficient,
      file = x$file,
      data_type = x$data_type,
      type = x$type,
      dt = list(x$dt)
    )
  }))

  names(tib_data$label) <- tib_data$header
  names(tib_data$type) <- tib_data$header
  lapply(tib_data$dt, data.table::setkey)
  names(tib_data$dt) <- tib_data$header

  return(tib_data)
}

#' @export
.build_tibble.set <- function(ls_,
                              sets) {

  ls_ <- lapply(ls_, function(header) {
    dim_length <- length(dimnames(header$data))
    header$dt <- data.table::as.data.table(as.matrix(header$data))
    data.table::setnames(header$dt, new = header$header)
    return(header)
  })

  l_idx <- match(names(ls_), sets$header)
  ls_ <- purrr::map2(ls_, l_idx, function(h, id) {
    h$file <- sets$file[id]
    return(h)
  })

  tib_data <- purrr::list_rbind(purrr::map(ls_, function(x) {
    tibble::tibble(
      header = x$header,
      label = x$label,
      coefficient = x$coefficient,
      file = x$file,
      data_type = "set",
      type = "character",
      dt = list(x$dt)
    )
  }))

  names(tib_data$label) <- tib_data$header
  names(tib_data$type) <- tib_data$header
  lapply(tib_data$dt, data.table::setkey)
  names(tib_data$dt) <- tib_data$header
  
  return(tib_data)
}
