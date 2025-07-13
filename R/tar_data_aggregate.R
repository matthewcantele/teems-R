#' @importFrom purrr pluck map2
#' 
#' @keywords internal
#' @noRd
.aggregate_data <- function(tib_data,
                            sets) {
  tib_data$dt <- purrr::map2(
    tib_data$dt,
    tib_data$data_type,
    function(dt, type) {
      if (type %=% "dat") {
        sets <- setdiff(colnames(dt), "Value")
        dt <- dt[, .(Value = sum(Value)), by = sets]
      } else if (type %=% "par") {
        if (all(c("sigma", "Weight") %in% colnames(dt))) {
          sets <- setdiff(colnames(dt), c("Value", "Weight", "sigma"))
          dt <- dt[, lapply(.SD, FUN = sum), .SDcols = c("Value", "Weight", "sigma"), by = sets]
          dt$Value <- dt$sigma / dt$Weight
          dt[is.nan(Value), let(Value = 1)] # for CGDS, return here
          dt[, let(sigma = NULL, Weight = NULL)]
        } else {
          # need a more systematic approach here
          # need weights for RWQH and RWQF gdyn headers
          sets <- setdiff(colnames(dt), "Value")
          if (!sets %=% character(0)) {
            dt <- dt[, .(Value = mean(Value)), by = sets]
          }
        }
      }
      return(dt)
    }
  )
  
  # add intertemporal sets
  if (any(sets$intertemporal)) {
    int_sets <- toupper(subset(
      sets,
      intertemporal,
      name
    )[[1]])

    tib_data$dt <- purrr::pmap(
      .l = list(
        tib_data$dt,
        tib_data$coefficient,
        tib_data$ls_upper_idx,
        tib_data$ls_mixed_idx
      ),
      .f = function(dt, nme, upper, mixed) {
        if (any(upper %in% int_sets)) {
          int_set <- intersect(upper, int_sets)
          non_int_set <- setdiff(colnames(dt), "Value")
          time_steps <- purrr::pluck(sets, "mapped_ele", int_set)
          dt <- dt[, .(time_steps, Value), by = non_int_set]
          data.table::setnames(
            dt,
            c(non_int_set, "time_steps"),
            mixed
          )
        }
        return(dt)
      }
    )
  }

  return(tib_data)
}
