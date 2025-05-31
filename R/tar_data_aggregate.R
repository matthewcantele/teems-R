#' @importFrom purrr pluck map2
#' 
#' @keywords internal
#' @noRd
.aggregate_data <- function(tib_data,
                            sets) {

  tib_data[["dt"]] <- purrr::map2(
    .x = tib_data[["dt"]],
    .y = tib_data[["data_type"]],
    .f = function(dt, type) {
      if (identical(x = type, y = "dat")) {
        sets <- setdiff(x = colnames(x = dt), y = "Value")
        dt <- dt[, .(Value = sum(Value)), by = sets]
      } else if (identical(x = type, y = "par")) {
        if (all(is.element(el = c("sigma", "Weight"), set = colnames(x = dt)))) {
          sets <- setdiff(x = colnames(x = dt), y = c("Value", "Weight", "sigma"))
          dt <- dt[, lapply(X = .SD, FUN = sum), .SDcols = c("Value", "Weight", "sigma"), by = sets]
          dt[["Value"]] <- dt[["sigma"]] / dt[["Weight"]]
          dt[is.nan(x = Value), let(Value = 1)] # for CGDS, return here
          dt[, let(sigma = NULL, Weight = NULL)]
        } else {
          # need a more systematic approach here
          # need weights for RWQH and RWQF gdyn headers
          sets <- setdiff(x = colnames(x = dt), y = "Value")
          if (!identical(x = character(0), y = sets)) {
            dt <- dt[, .(Value = mean(Value)), by = sets]
          }
        }
      }
      return(dt)
    }
  )

  # add intertemporal sets
  if (any(sets[["intertemporal"]])) {
    int_sets <- toupper(x = subset(
      x = sets,
      subset = intertemporal,
      select = name
    )[[1]])

    tib_data[["dt"]] <- purrr::pmap(.l = list(tib_data[["dt"]],
                                              tib_data[["coefficient"]],
                                              tib_data[["ls_upper_idx"]],
                                              tib_data[["ls_mixed_idx"]]),
                                    .f = function(dt, nme, upper, mixed) {
                                      if (any(is.element(el = upper, set = int_sets))) {
                                        int_set <- intersect(x = upper, y = int_sets)
                                        non_int_set <- setdiff(x = colnames(x = dt), y = "Value")
                                        time_steps <- purrr::pluck(.x = sets, "mapped_ele", int_set)
                                        dt <- dt[, .(time_steps, Value), by = non_int_set]
                                        data.table::setnames(x = dt,
                                                             old = c(non_int_set, "time_steps"),
                                                             new = mixed)
                                      }
                                      return(dt)
                               })
  }

  return(tib_data)
}
