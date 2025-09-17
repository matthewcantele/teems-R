#' @importFrom data.table copy rbindlist let setnames
#' 
#' @keywords internal
#' @noRd
.weight_param <- function(i_data,
                          data_format) {

  weight_map <- switch(data_format,
                       "v6.2" = param_weights$v6.2,
                       "v7.0" = param_weights$v7.0)

  weight_headers <- gsub("-", "", unique(unlist(weight_map)))
  weights <- data.table::copy(i_data[names(i_data) %in% weight_headers])

  if (data_format %=% "v7.0") {
    data.table::setnames(weights$ISEP, old = "COMM", new = "ACTS")
  }

  flip_headers <- sub("-", "", grep("-", unlist(weight_map), value = TRUE))
  weights <- lapply(weights, function(w) {
    if (inherits(w, flip_headers)) {
      w[, let(Value = Value * -1)]
    }
    return(w)
  })

  weight_map <- lapply(weight_map, gsub, pattern = "-", replacement = "")

  i_data <- lapply(i_data, function(h) {
    if (inherits(h, names(weight_map))) {
      w_headers <- weight_map[[class(h)[1]]]
      ls_w <- weights[w_headers]
      sets <- colnames(h)[!colnames(h) %in% "Value"]
      w <- data.table::rbindlist(lapply(ls_w, function(weight) {
        weight[, .(Value = sum(Value)), by = sets]
      }))[, .(omega = sum(Value)), by = sets]
      h <- merge(h, w, sets)
      h[, let(sigma = Value * omega)]
    }
    return(h)
  })
 
  return(i_data)
}
