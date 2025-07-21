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

  chron_yrs_set <- setdiff(colnames(chron_yrs), "Value")
  # only headers with a time set
  if (any(colnames(data) %in% int_sets)) {
    # get time set (there are multiple)
    TIMEt <- colnames(x = data)[is.element(el = colnames(x = data), set = int_sets)]
    # convert time sets to numeric
    data[, (TIMEt) := as.integer(x = get(x = TIMEt))]
    # bring over chronological years
    r_idx <- match(data[[TIMEt]], chron_yrs[[chron_yrs_set]])
    data[, Year := chron_yrs$Value[r_idx]]
    return(data)
  } else {
    return(data)
  }
}
