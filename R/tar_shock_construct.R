#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.shock_construct <- function(shocks,
                             closure,
                             var_extract,
                             sets,
                             reference_year) {

  final_shocks <- lapply(X = shocks,
                         FUN = function(shk) {
                           if (identical(x = shk[["type"]], y = "uniform")) {
                             f_shk <- .uniform_shock(raw_shock = shk,
                                                     closure = closure,
                                                     var_extract = var_extract,
                                                     sets = sets)
                           } else if (is.element(el = shk[["type"]], set = c("custom", "scenario"))) {
                             if (identical(x = shk[["type"]], y = "scenario")) {
                               browser()
                               .convert_scenario(raw_shock = shk,
                                                 reference_year = reference_year,
                                                 sets = sets,
                                                 var_extract = var_extract,
                                                 YEAR = attr(x = sets, which =  "CYRS")) 
                             }
                             f_shk <- .custom_shock(raw_shock = shk,
                                                    closure = closure,
                                                    var_extract = var_extract,
                                                    sets = sets)
                           } 
                           return(f_shk)
                         })
  browser()
  final_shocks <- unlist(x = final_shocks, recursive = F)
  # note to swap purrr map functions for all sapplys and some lapplys eventually
  # get name for shock input
  shock_names <- purrr::map_chr(.x = shocks, .f = "var")
  if (length(x = final_shocks) < 4) {
    shock_id <- paste(substring(text = shock_names,
                                first = 1,
                                last = 2), collapse = "_")
  } else {
    shock_id <- paste(substring(text = shock_names,
                                first = 1,
                                last = 1), collapse = "")
  }

  shock_file <- paste0(paste(paste(shock_id, format(x = Sys.time(), "%H%M"), sep = "_"), collapse = "_"), ".shf")

  shock_list <- list(
    shocks = final_shocks,
    shock_file = shock_file
  )
  
  return(shock_list)
}