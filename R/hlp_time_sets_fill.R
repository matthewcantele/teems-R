#' @importFrom data.table data.table
#' @importFrom tibble tibble
#' 
#' @keywords internal
#' @noRd
.fill_time_sets <- function(time_steps,
                            time_sets) {

  n_timestep <- length(x = time_steps) + 1
  time_sets[["dt"]] <- lapply(X = time_sets[["definition"]],
                              FUN = function(def) {
                                num_vec <- .parse_tab_int(expr = def,
                                                          ninterval = n_timestep)
                                dt <- data.table::data.table(Value = num_vec)
                                return(dt)
                              })

  time_sets[["information"]] <- trimws(x = gsub(pattern = "#",
                                                replacement =  "",
                                                x = time_sets[["information"]]))

  int_sets <- tibble::tibble(header = time_sets[["name"]],
                             information = time_sets[["information"]],
                             type = "integer",
                             aggregate = FALSE,
                             input_file = time_sets[["file"]],
                             dt = time_sets[["dt"]],
                             full_sets = NA,
                             set_name = time_sets[["name"]])
  
  names(x = int_sets[["dt"]]) <- int_sets[["header"]]
  return(int_sets)
}
