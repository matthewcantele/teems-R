#' @keywords internal
#' @noRd
.match_year <- function(data,
                        sets,
                        chron_yrs) {
  int_sets <- toupper(x = unlist(x = subset(
    x = sets,
    subset = {
      is.element(el = intertemp, set = 1)
    },
    select = setname
  )))

  int_sets <- paste0(int_sets, "t")
  # only headers with a time set
  if (any(is.element(el = colnames(x = data), set = int_sets))) {
    # get time set (there are multiple)
    TIMEt <- colnames(x = data)[is.element(el = colnames(x = data), set = int_sets)]
    # convert time sets to numeric
    data[, (TIMEt) := as.integer(x = get(x = TIMEt))]
    # bring over chronological years
    r_idx <- match(x = data[[TIMEt]], table = chron_yrs[["ALLTIMEt"]])
    data[["Year"]] <- chron_yrs[["Value"]][r_idx]
    return(data)
  } else {
    return(data)
  }
}
