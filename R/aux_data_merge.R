#' Data Merge for Pre and Post Coefficients
#'
#' Merges pre-model and post-model coefficient data based on specified criteria,
#' handling intertemporal data by reducing it to a single pre-model time step
#' entry and renaming the step. It matches coefficients with a reference table,
#' sets the year to a base year, renames coefficients, filters post-model data
#' by pre-model data, checks for consistency, and finally merges the data. It
#' also distinguishes between pre and post "Value" columns and calculates the
#' percentage change for non-intertemporal data.
#'
#' @inheritParams .build_tibble
#'
#' @param pre_coeff A list or data frame containing pre-model coefficients.
#' @param post_coeff A list or data frame containing post-model coefficients.
#' @param tab_coeff A list or data frame containing the reference table for
#'   matching coefficients.
#' @param sets A list containing set definitions and their elements.
#' @param intertemporal Logical; whether intertemporal.
#'
#' @return A list containing the merged pre-model and post-model data, with
#'   additional processing for intertemporal data and calculation of percentage
#'   changes for non-intertemporal data.
#' @importFrom purrr map2
#' @importFrom data.table merge.data.table setkey rbindlist setcolorder
#' @keywords internal
#' @noRd
.merge_data <- function(pre_coeff,
                        post_coeff,
                        sets,
                        tab_coeff,
                        reference_year,
                        intertemporal) {
  # use elaborated set names to match
  r_idx <- match(x = names(x = pre_coeff), table = tab_coeff[["header"]])

  # first check on the merge
  if (!identical(x = length(x = r_idx), y = length(x = pre_coeff))) {
    stop("Index/coefficient length mismatch indicates Tab parsing error.")
  }
  # set "Year" to base year of reference data
  # set ALLTIME in premodel to -1
  pre_coeff <- lapply(X = pre_coeff, FUN = function(dt) {
    if (intertemporal) {
      dt <- dt[ALLTIMEt == 0]
      dt[, let(ALLTIMEt = -1)]
    }
    dt[, let(Year = ..reference_year)]
    return(dt)
  })

  # rename pre_coeff names from headers to actual names (e.g., VTWR from VTMFSD)
  r_idx <- match(x = names(x = pre_coeff), table = tab_coeff[["header"]])
  names(x = pre_coeff) <- tab_coeff[["name"]][r_idx]

  # filter post_coeff data by pre_coeff
  header_nmes <- unlist(x = subset(
    x = tab_coeff,
    subset = {
      is.element(el = name, set = names(x = pre_coeff))
    },
    select = "name"
  ), use.names = FALSE)

  # get basedata coefficients from output data using
  post_base <- subset(x = post_coeff, subset = {
    is.element(el = name, set = header_nmes)
  })

  # check that all pre_coeff data exists in post_coeff
  if (!identical(
    x = character(0),
    y = setdiff(x = names(x = pre_coeff), y = post_base[["name"]])
  )) {
    stop("Set difference found on pre/post model header names.")
  }

  # and merge data
  r_idx <- match(x = post_base[["name"]], table = names(x = pre_coeff))
  post_base[["pre_dat"]] <- pre_coeff[r_idx]

  if (intertemporal) {
    # final dt merge
    post_base[["final_dat"]] <- purrr::map2(
      .x = post_base[["pre_dat"]],
      .y = post_base[["dat"]],
      .f = function(pre, post) {
        data.table::rbindlist(list(pre, post))
      }
    )

    # include percentage change and move "Year" column
    lapply(X = post_base[["final_dat"]],
           FUN = function(dt) {
      data.table::setcolorder(dt, "Year", after = ncol(x = dt) - 1)
      data.table::setkey(x = dt)
      return(dt)
    })
  } else {
    # add "Year" indicator for post-model data and stack
    post_base[["final_dat"]] <- purrr::map2(
      .x = post_base[["pre_dat"]],
      .y = post_base[["dat"]],
      .f = function(pre, post) {
        post[, Year := paste("post", reference_year, sep = "_")]
        pre[, Year := paste("ante", as.character(x = Year), sep = "_")]
        final <- data.table::rbindlist(l = list(pre, post))
        data.table::setcolorder(x = final, "Value", after = ncol(x = final))
        return(final)
      }
    )

    lapply(X = post_base[["final_dat"]],
           FUN = data.table::setkey)
  }

  post_base <- subset(x = post_base, select = -c(pre_dat, dat))
  return(post_base)
}
