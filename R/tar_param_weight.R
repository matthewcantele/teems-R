#' Weight Parameters for Model
#'
#' This function adjusts weights for model parameters based on a mapping and weights provided. It handles
#' negative weights, renames columns for parameter weighting, and calculates final weights for parameters.
#' Additionally, it can adjust parameters based on rate of return flexibility and delta values if provided.
#'
#' @inheritParams .set_specs
#' @inheritParams .retrieve_mappings
#' @inheritParams .aggregate_data
#' @inheritParams teems_parameters
#'
#' @param weights A data frame of weights with headers.
#' @param par The parameters data frame to be weighted.
#'
#' @importFrom purrr pluck map2
#' @importFrom data.table copy rbindlist let
#' @importFrom tibble tibble
#' @return A data frame of parameters with adjusted weights.
#' @keywords internal
#' @noRd
.weight_param <- function(weights,
                          RDLT,
                          par,
                          data_format) {

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
  weights <- subset(x = weights,
                    subset = {is.element(el = header, set = weight_headers)})

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
  par_weights <- subset(x = par,
                        subset = {is.element(el = header, set = weight_map[["header"]])})

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
        data.table::rbindlist(l = lapply(X = ls_weights, FUN = function(weight) {
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

  # weighted elasticities
  par_weights <- subset(x = par_weights, select = c("header", "information", "coeff", "v_class", "input_file"))

  # place elasticities ready for aggregation into main prm object
  r_idx <- match(x = names(x = elasticities), table = par_weights[["header"]])
  par_weights[["dt"]][r_idx] <- elasticities
  names(x = par_weights[["dt"]]) <- par_weights[["header"]]

  # all other parameters
  other_par <- subset(x = par, !is.element(el = header, set = par_weights[["header"]]))

  # consolidated
  par <- rbind(par_weights, other_par)


  return(par)
}
