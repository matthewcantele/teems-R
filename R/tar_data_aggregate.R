#' @importFrom purrr pluck map2
#' 
#' @keywords internal
#' @noRd
.aggregate_data <- function(tib_data,
                            sets,
                            coeff_extract) {
  
  req_headers <- unlist(x = subset(
    x = coeff_extract,
    subset = {!is.na(x = file)},
    select = header
  ))
  
  if (!all(is.element(el = req_headers, set = tib_data[["header"]]))) {
    missing_headers <- toString(x = req_headers[!is.element(el = req_headers, set = tib_data[["header"]])])
    stop(paste(
      "Headers determined as required:",
      missing_headers,
      "were not loaded."
    ))
  }
  # remove unnecessary (not written) headers
  tib_data <- subset(
    x = tib_data,
    subset = {
      is.element(el = header, set = req_headers)
    }
  )
  
  # merge required coeff_extract information
  r_idx <- match(x = tib_data[["coefficient"]],
                 table = coeff_extract[["coefficient"]])
  
  tib_data[["ls_upper_idx"]] <- coeff_extract[["ls_upper_idx"]][r_idx]
  tib_data[["ls_mixed_idx"]] <- coeff_extract[["ls_mixed_idx"]][r_idx]
  
  # mappings
  tib_data[["dt"]] <- lapply(X = tib_data[["dt"]],
                             FUN = .preagg_map,
                             sets = sets)
  
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

  return(tib_data)
}
