#' @importFrom purrr pluck map2
#' @importFrom data.table copy rbindlist let key setkey
#' @importFrom tibble tibble
#' @return A data frame of parameters with adjusted weights.
#' @keywords internal
#' @noRd
.weight_param <- function(tib_data,
                          data_format,
                          sets) {

  # the chances of someone providing all parameter values is low so we will
  # do the weighted mappings and then swap out final dts for any user-specific
  if (identical(x = data_format, y = "v6.2")) {
    weight_map <- param_weights[["v6.2"]]
  } else if (identical(x = data_format, y = "v7.0")) {
    weight_map <- param_weights[["v7.0"]]
  }

  # get all headers used for weights and drop "-"
  weight_headers <- gsub(pattern = "-", "", unique(x = unlist(x = weight_map)))

  # get weight header data
  weights <- data.table::copy(subset(x = tib_data,
                    subset = {is.element(el = header, set = weight_headers)}))

  int_sets <- subset(sets, intertemporal, name)[[1]]
  
  # strip time sets for weight calculations
  weights$dt <- lapply(weights$dt,
                    function(dt) {
                      c_idx <- match(int_sets, .dock_tail(colnames(dt)))
                      if (any(!is.na(c_idx))) {
                        c_idx <- purrr::discard(c_idx, is.na)
                        dt[, (c_idx) := NULL]
                        dt <- unique(dt)
                      }
                      return(dt)
                    })

  # merge weights into parameter tibble
  # no idea why setnames will not work here
  # data.table::setnames(weights[['dt']][['ISEP']], 'COMM', 'ACTS')
  # ISEP ACTS to COMM for parameter weighting (data not read into model)
  if (identical(x = data_format, y = "v7.0")) {
    colnames(x = purrr::pluck(.x = weights, "dt", "ISEP"))[which(colnames(x = purrr::pluck(.x = weights, "dt", "ISEP")) == "COMMc")] <- "ACTSa"
  }

  # headers used as weights
  w_headers <- unlist(x = weight_map)

  # flip negative weights with '-' as first character
  flip <- gsub(
    pattern = "-",
    replacement = "",
    x = unique(x = w_headers[substring(text = w_headers, first = 1, last = 1) == "-"])
  )

  weights[["dt"]] <- sapply(X = weights[["header"]], FUN = function(w) {
    dt <- purrr::pluck(.x = weights, "dt", w)
    if (is.element(el = w, set = flip)) {
      .dt <- data.table::copy(x = dt)
      .dt[, let(Value = Value * -1)]
      return(.dt)
    } else {
      return(dt)
    }
  })

  # drop '-' from weight src
  weight_map <- sapply(X = weight_map, FUN = function(h) {
    gsub(pattern = "-", replacement = "", x = h)
  })

  r_idx <- lapply(X = weight_map, FUN = function(w) {
    match(x = w, table = weights[["header"]])
  })

  # join parameters with associated weights
  weight_map <- tibble::tibble(
    header = names(x = weight_map),
    w_dt = sapply(X = r_idx, FUN = function(r) {
      weights[["dt"]][r]
    })
  )

  # pull weighted parameters
  par_weights <- data.table::copy(subset(x = tib_data,
                        subset = {is.element(el = header, set = weight_map[["header"]])}))

  par_weights$dt <- lapply(par_weights$dt,
                       function(dt) {
                         c_idx <- match(int_sets, .dock_tail(colnames(dt)))
                         if (any(!is.na(c_idx))) {
                           c_idx <- purrr::discard(c_idx, is.na)
                           dt[, (c_idx) := NULL]
                           dt <- unique(dt)
                         }
                         return(dt)
                       })
  # combine all data in single object
  r_idx <- match(x = par_weights[["header"]], table = weight_map[["header"]])
  par_weights["weights"] <- weight_map[r_idx, "w_dt"]

  # calculate final weights
  par_weights[["final_weights"]] <-
    purrr::map2(
      .x = par_weights[["weights"]],
      .y = par_weights[["dt"]],
      .f = function(ls_weights, par_header) {
        sets <- setdiff(x = colnames(x = par_header), y = "Value")
        # adjust indexing on weights
        data.table::rbindlist(l = lapply(X = ls_weights, FUN = function(weight) {
          if (is.element(el = "ACTSs", set = colnames(x = weight))) {
            data.table::setnames(x = weight, old = "ACTSs", new = "ACTSa")
          }
          weight[, .(Value = sum(Value)), by = sets]
        }))[, .(Weight = sum(Value)), by = sets]
      }
    )

  # merge with original parameter values and calculate weighted values
  elasticities <- purrr::map2(
    .x = par_weights[["dt"]],
    .y = par_weights[["final_weights"]],
    .f = function(orig_values, weights) {
      sets <- colnames(x = orig_values)[x = colnames(x = orig_values) != "Value"]
      dt <- merge(
        x = orig_values,
        y = weights,
        by = sets
      )
      dt[, let(sigma = Value * Weight)]
    }
  )

  # place elasticities ready for aggregation into main prm object
  r_idx <- match(tib_data[["header"]], names(elasticities))
  tib_data$dt <- purrr::map2(tib_data$dt,
              r_idx,
              function(dt, id) {
                if (!is.na(id)) {
                  e <- elasticities[[id]]
                  new_key <- c(data.table::key(e), "Value")
                  data.table::setkeyv(e, new_key)
                  dt <- merge(dt, e)
                }
                return(dt)
              })

  return(tib_data)
}
