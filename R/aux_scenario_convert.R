#' @importFrom purrr pluck
.convert_scenario <- function(input,
                              time_coeff,
                              sets) {

  # convert CYRS to ALLTIMEt
  # t0 timestep to remain unshocked
  chron_yrs <- purrr::pluck(.x = time_coeff, "dt", "CYRS")
  # check that chronological years are provied
  if (!is.element(el = "CYRS", set = colnames(x = input))) {
    stop(
      paste(
        dQuote(x = "scenario"),
        "type shocks must contain a column CYRS representing chronological years."
      )
    )
  } else if (!all(is.element(el = chron_yrs[["Value"]], set = input[["CYRS"]]))) {
    missing_yrs <- chron_yrs[["Value"]][!is.element(
      el = chron_yrs[["Value"]],
      set = input[["CYRS"]]
    )]
    stop(
      paste(
        "The scenario file provided does not contain the following years:",
        missing_yrs
      )
    )
  }

  # grab the requiste chronological years from the trajectory and convert to timesteps
  input <- subset(
    x = input,
    subset = {
      is.element(
        el = CYRS,
        set = chron_yrs[["Value"]]
      )
    }
  )

  r_idx <- match(x = input[["CYRS"]], table = chron_yrs[["Value"]])
  input[["ALLTIMEt"]] <- chron_yrs[["ALLTIMEt"]][r_idx]
  input[, CYRS := NULL]

  set_maps <- subset(
    x = sets,
    subset = {
      !is.na(x = names(x = sets[["full_sets"]]))
    },
    select = full_sets
  )

  set_maps <- data.table::rbindlist(l = set_maps[["full_sets"]], use.names = FALSE)
  data.table::setnames(x = set_maps, new = c("origin", "map"))

  # this can eventually be made into a function
  input <- input[, lapply(.SD, FUN = function(char_col) {
    if (is.character(x = char_col)) {
      r_idx <- match(x = char_col, table = set_maps[["origin"]])
      char_col <- set_maps[["map"]][r_idx]
    }
    return(char_col)
  })]

  # aggregate
  user_sets <- setdiff(x = colnames(x = input), y = "Value")
  input <- input[, .(Value = sum(Value)), by = user_sets]

  data.table::setorder(x = input)

# it's either all ALLTIMEt or not
  # non-Value, non-int columns
  # write function to go between REG to REGr for all sets
  non_int_sets <- toupper(x = unlist(x = subset(x = sets, subset = {
    is.element(el = qualifier, set = "(non_intertemporal)")
  }, select = name)))
  int_sets <- toupper(x = unlist(x = subset(x = sets, subset = {
    is.element(el = qualifier, set = "(intertemporal)")
  }, select = name)))
  non_int_value_col <- colnames(x = input)[is.element(
    el = sub(pattern = ".{1}$", replacement = "", x = colnames(x = input)),
    set = non_int_sets
  )]

  int_value_col <- colnames(x = input)[is.element(
    el = sub(pattern = ".{1}$", replacement = "", x = colnames(x = input)),
    set = int_sets
  )]

  # calculate the percentage change
  # recursive approach
  # value[, Value := (Value - data.table::shift(Value)) / data.table::shift(Value) * 100, by = non_int_value_col]
  # NAs to 0
  # value[is.na(Value), Value := 0]
  # intertemporal
  input[, Value := (Value - Value[get(x = int_value_col) == 0]) / Value[get(x = int_value_col) == 0] * 100, by = non_int_value_col]

  return(input)
}
