#' @importFrom purrr map_chr map_lgl list_flatten
#'
#' @keywords internal
#' @noRd
.shock_load <- function(shocks,
                        closure,
                        sets,
                        var_extract) {

  final_shocks <- lapply(
    X = shocks,
    FUN = function(shk) {
      .construct_shock(
        raw_shock = shk,
        closure = closure,
        sets = sets,
        var_extract = var_extract
      )
    }
  )

  final_shocks <- unlist(x = final_shocks, recursive = F)
  shock_names <- purrr::map_chr(.x = shocks, .f = "var")
  
  if (length(x = final_shocks) < 4) {
    shock_id <- paste(substring(
      text = shock_names,
      first = 1,
      last = 2
    ), collapse = "_")
  } else {
    shock_id <- paste(substring(
      text = shock_names,
      first = 1,
      last = 1
    ), collapse = "")
  }

  file <- paste0(paste(paste(shock_id, format(x = Sys.time(), "%H%M"), sep = "_"), collapse = "_"), ".shf")
  attr(final_shocks, "file") <- file

  return(final_shocks)
}