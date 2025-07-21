#' @importFrom purrr map2
#' @importFrom data.table setkey rbindlist setcolorder
#' 
#' @keywords internal
#' @noRd
.merge_data <- function(pre_coeff,
                        post_coeff,
                        sets,
                        coeff_extract,
                        reference_year,
                        intertemporal,
                        call) {

  # use elaborated set names to match
  r_idx <- match(x = names(x = pre_coeff), table = coeff_extract[["header"]])

  # first check on the merge
  if (!identical(x = length(x = r_idx), y = length(x = pre_coeff))) {
    .cli_action(msg = c("Index/coefficient length mismatch indicates Tab parsing error.",
                        "Contact the developer."),
                action = c("abort", "inform"),
                call = call)
  }

  # set "Year" to base year of reference data
  # set ALLTIME in premodel to -1
  if (intertemporal) {
    int_sets <- subset(sets, intertemporal, name)[[1]]
    pre_coeff <- lapply(X = pre_coeff, FUN = function(dt) {
      stnd_names <- .dock_tail(string = colnames(x = dt))
      if (any(is.element(el = int_sets, set = stnd_names))) {
        int_set <- colnames(x = dt)[is.element(el = stnd_names, set = int_sets)]
        dt <- dt[get(x = int_set) == 0]
        dt[[int_set]] <- -1
        dt[["Year"]] <- reference_year
      }
      return(dt)
    })
  }

  # rename pre_coeff names from headers to actual names (e.g., VTWR from VTMFSD)
  r_idx <- match(x = names(x = pre_coeff), table = coeff_extract[["header"]])
  names(x = pre_coeff) <- coeff_extract[["coefficient"]][r_idx]

  # get basedata coefficients from output data using
  post_base <- subset(x = post_coeff, subset = {
    is.element(el = coefficient, set = names(x = pre_coeff))
  })

  # check that all pre_coeff data exists in post_coeff
  if (!identical(
    x = character(0),
    y = setdiff(x = names(x = pre_coeff), y = post_base[["coefficient"]])
  )) {
    .cli_action(msg = c("Set difference found on pre/post model header names.",
                        "This is an internal error, contact dev"),
                action = c("abort", "inform"),
                call = call)
  }

  # and merge data
  r_idx <- match(x = post_base[["coefficient"]], table = names(x = pre_coeff))
  post_base[["pre_dat"]] <- pre_coeff[r_idx]

  if (intertemporal) {
    # final dt merge
    post_base[["dat"]] <- purrr::map2(
      .x = post_base[["pre_dat"]],
      .y = post_base[["dat"]],
      .f = function(pre, post) {
        data.table::rbindlist(list(pre, post))
      }
    )

    # include percentage change and move "Year" column
    lapply(
      X = post_base[["dat"]],
      FUN = function(dt) {
        if (is.element(el = "Year", set = colnames(dt))) {
          data.table::setcolorder(dt, "Value", after = ncol(dt))
        }
        data.table::setkey(x = dt)
        return(dt)
      }
    )
  } else {
    # add "Year" indicator for post-model data and stack
    post_base[["dat"]] <- purrr::map2(
      .x = post_base[["pre_dat"]],
      .y = post_base[["dat"]],
      .f = function(pre, post) {
        post[, Year := paste("post", reference_year, sep = "_")]
        pre[, Year := paste("ante", reference_year, sep = "_")]
        final <- data.table::rbindlist(l = list(pre, post))
        data.table::setcolorder(x = final, "Value", after = ncol(x = final))
        final[["Value"]] <- as.numeric(x = final[["Value"]])
        return(final)
      }
    )

    lapply(
      X = post_base[["dat"]],
      FUN = data.table::setkey
    )
  }

  post_base <- subset(x = post_base, select = -pre_dat)
  return(post_base)
}
