#' @importFrom purrr map2
#' @importFrom data.table as.data.table
#' @importFrom tibble tibble
#' 
#' @keywords internal
#' @noRd
.fill_time_sets <- function(n_timestep,
                            n_timestep_coeff,
                            int_sets) {

  int_sets[["dt"]] <- purrr::map2(.x = int_sets[["definition"]],
                                  .y = int_sets[["name"]],
                                  .f = function(def, nme) {
                                    num_vec <- .parse_tab_int(
                                      expr = def,
                                      n_timestep = n_timestep,
                                      n_timestep_coeff = n_timestep_coeff
                                    )
                                    names(x = num_vec) <- nme
                                    dt <- data.table::as.data.table(x = num_vec)
                                    data.table::setkey(x = dt)
                                  })

  int_sets[["label"]] <- trimws(x = gsub(pattern = "#",
                                         replacement =  "",
                                         x = int_sets[["label"]]))

  int_sets <- tibble::tibble(header = int_sets[["name"]],
                             label = int_sets[["label"]],
                             coefficient = NA,
                             file = int_sets[["file"]],
                             data_type = "set",
                             type = "integer",
                             dt = int_sets[["dt"]])
  
  names(x = int_sets[["dt"]]) <- int_sets[["header"]]
  return(int_sets)
}
