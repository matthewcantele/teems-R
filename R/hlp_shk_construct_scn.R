#' @importFrom data.table fread CJ setnames fsetdiff
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @importFrom rlang abort
#' 
#' @noRd
#' @keywords internal
#' @export
.construct_shock.scenario <- function(raw_shock,
                                      closure,
                                      sets) {
  value <- data.table::fread(raw_shock$input)

  full_set_ele <- with(sets$full_ele, mget(raw_shock$ls_upper))
  template_shk <- do.call(data.table::CJ, c(full_set_ele, sorted = FALSE))
  time_set_upper <- intersect(raw_shock$ls_upper, subset(sets, intertemporal, name)[[1]])
  time_set_ele <- purrr::pluck(sets, "mapped_ele", time_set_upper)
  CYRS <- attr(sets, "CYRS")
  CYRS <- tibble::tibble(
    YEAR = CYRS,
    time_set_ele
  )

  if (.o_check_shock_status()) {
    r_idx <- match(template_shk[[time_set_upper]], CYRS$time_set_ele)
    template_shk[[time_set_upper]] <- CYRS$YEAR[r_idx]
    data.table::setnames(template_shk, new = raw_shock$set)
    browser()
    #fsetequal
    if (!nrow(data.table::fsetdiff(template_shk, value[, !"Value"])) %=% 0L) {
      missing_tuples <- data.table::fsetdiff(template_shk, value[, !"Value"])
      n_missing_tuples <- nrow(missing_tuples)
      missing_tuples <- capture.output(print(missing_tuples))[-c(1, 2)]
      error_fun <- substitute(.cli_action(
        shk_err$scen_missing_tup,
        action = c("abort", "inform"),
        call = call
      ))

      error_var <- substitute(variables <- list(
        missing_tuples = missing_tuples,
        n_missing_tuples = n_missing_tuples
      ))

      error_inputs <- .pipeline_error(
        error_var = error_var,
        error_fun = error_fun,
        call_id = attr(raw_shock, "call_id")
      )

      rlang::abort(error_inputs)
    }
  }
  value <- subset(value, Year %in% CYRS$YEAR)
  time_set <- raw_shock$ls_mixed[match(time_set_upper, raw_shock$ls_upper)]
  colnames(CYRS)[2] <- time_set
  r_idx <- match(value$Year, CYRS$YEAR)
  value$Year <- CYRS[[time_set]][r_idx]
  data.table::setnames(value, "Year", time_set)
  r_idx <- match("Year", raw_shock$set)
  raw_shock$set[r_idx] <- time_set
  value <- .preagg_map(dt = value, sets = sets)
  mapped_sets <- setdiff(colnames(value), "Value")
  value <- value[, .(Value = sum(Value)), by = mapped_sets]

  int_col <- setdiff(
    colnames(value)[unlist(value[, lapply(.SD, is.integer)])],
    "Value"
  )
  non_int_col <- setdiff(
    colnames(value),
    c(int_col, "Value")
  )

  value[, Value := {
    baseline <- Value[get(int_col) == 0]
    (Value - baseline) / baseline * 100
  }, by = non_int_col]


  raw_shock$input <- value
  class(raw_shock) <- "custom"

  .construct_shock(
    raw_shock = raw_shock,
    closure = closure,
    sets = sets
  )
}
