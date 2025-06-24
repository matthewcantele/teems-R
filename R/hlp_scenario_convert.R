#' @importFrom data.table rbindlist setnames setorder
#' 
#' @keywords internal
#' @noRd
.convert_scenario <- function(raw_shock,
                              reference_year,
                              sets,
                              var_extract,
                              YEAR) {
browser()
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
  time_set_stnd <- substr(x = time_set, start = 1, stop = nchar(x = time_set) - 1)
  time_set_ele <- purrr::pluck(.x = sets, "mapped_ele", time_set_stnd)
  CYRS <- tibble::tibble(YEAR = YEAR,
                         time_set_ele)
  colnames(x = CYRS)[2] <- time_set
  r_idx <- match(x = value[["Year"]], table = CYRS[["YEAR"]])
  value[["Year"]] <- CYRS[[time_set]][r_idx]
  data.table::setnames(x = value, old = "Year", new = time_set)
  #here
  raw_shock[["set"]] <- sub(pattern = "Year", replacement = time_set, x = raw_shock[["set"]])
  names(x = raw_shock[["ele"]]) <- raw_shock[["set"]]
  
  YEAR[["Year"]] <- chron_yrs
  r_idx <- match(x = input[["Year"]], table = YEAR[["Year"]])
  input[["ALLTIMEt"]] <- YEAR[["ALLTIMEt"]][r_idx]
  input[, Year := NULL]

  set_maps <- subset(
    x = sets,
    subset = {
      !is.na(x = names(x = sets[["mapping"]]))
    },
    select = mapping
  )

  set_maps <- data.table::rbindlist(l = set_maps[["mapping"]], use.names = FALSE)
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

  non_int_sets <- toupper(x = unlist(x = subset(x = sets, subset = !intertemporal, select = name)))
  int_sets <- toupper(x = unlist(x = subset(x = sets, subset = intertemporal, select = name)))
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
