#' @importFrom data.table rbindlist setnames setorder
#' 
#' @keywords internal
#' @noRd
.convert_scenario <- function(raw_shock,
                              reference_year,
                              sets,
                              var_extract,
                              YEAR) {

  value <- data.table::fread(raw_shock[["file"]])
  
    if (!all(is.element(el = YEAR, set = value[["Year"]]))) {
      missing_yrs <- setdiff(x = YEAR, value[["Year"]])
      .cli_action(msg = "The scenario shock provided does not contain time steps 
                  associated with {.val {missing_yrs}}.",
                  action = "abort",
                  call = call)
    }

  # grab the requiste chronological years from the trajectory and convert to timesteps
  value <- subset(
    x = value,
    subset = {
      is.element(
        el = Year,
        set = YEAR
      )
    }
  )
  
  ls_mixed <- purrr::pluck(.x = var_extract, "ls_mixed_idx", raw_shock[["var"]])
  time_set <- setdiff(x = ls_mixed, y = raw_shock[["set"]])
  time_set_stnd <- .dock_tail(string = time_set)
  time_set_ele <- purrr::pluck(.x = sets, "mapped_ele", time_set_stnd)
  CYRS <- tibble::tibble(YEAR = YEAR,
                         time_set_ele)
  colnames(x = CYRS)[2] <- time_set
  r_idx <- match(x = value[["Year"]], table = CYRS[["YEAR"]])
  value[["Year"]] <- CYRS[[time_set]][r_idx]
  data.table::setnames(x = value, old = "Year", new = time_set)
  raw_shock[["set"]] <- sub(pattern = "Year", replacement = time_set, x = raw_shock[["set"]])
  #here
  ls_upper <- purrr::pluck(.x = var_extract, "ls_upper_idx", raw_shock[["var"]])
  
  set_ele <- with(
    data = sets[["full_ele"]],
    expr = mget(x = ls_upper)
  )
  
  template_shk <- do.call(
    what = data.table::CJ,
    args = c(set_ele, sorted = FALSE)
  )
  
  data.table::setnames(x = template_shk, new = ls_mixed)
  data.table::setkeyv(x = value, cols = head(x = names(x = value), -1))
  data.table::setkey(x = template_shk)
  
  if (!all(is.element(el = template_shk, set = value))) {
    missing_tuples <- data.table::fsetdiff(x = template_shk, y = value[, !"Value"])
    missing_tuples <- capture.output(print(x = missing_tuples))
    missing_tuples <- missing_tuples[-c(1, 2, 3)]
    .cli_action(msg = c("Some tuples designated for a {.arg scenario} shock are 
    missing: {.field {missing_tuples}}.",
                        "{.arg scenario} shocks must contain all 
                        database-specific preaggregation elements for 
                        associated sets."),
                action = c("abort", "inform"))
  }
  
  value <- .preagg_map(dt = value,
                       sets = sets)

  mapped_sets <- setdiff(x = colnames(x = value), y = "Value")
  value <- value[, .(Value = sum(Value)), by = mapped_sets]

  int_col <- setdiff(x = colnames(x = value)[unlist(x = value[, lapply(.SD, is.integer)])],
                     y = "Value")
  non_int_col <- setdiff(x = colnames(x = value),
                         y = c(int_col, "Value"))

  # calculate the percentage change
  # recursive approach
  # value[, Value := (Value - data.table::shift(Value)) / data.table::shift(Value) * 100, by = non_int_value_col]
  # NAs to 0
  # value[is.na(Value), Value := 0]
  # intertemporal

  value[, Value := {
    baseline <- Value[get(x = int_col) == 0]
    (Value - baseline) / baseline * 100
  }, by = non_int_col]
  
  raw_shock[["value"]] <- value
  raw_shock[["type"]] <- "custom"
  return(raw_shock)
}
