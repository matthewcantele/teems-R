#' @importFrom purrr map_chr map_lgl list_flatten
#'
#' @keywords internal
#' @noRd
.shock_load <- function(shocks,
                        closure,
                        sets,
                        var_extract) {

  multi_ele_shks <- lapply(shocks, function(s) {
    if (any(lengths(s$subset) > 1)) {
      ele_comb <- expand.grid(s$subset, stringsAsFactors = FALSE)
      purrr::map(
        .x = seq_len(nrow(ele_comb)),
        .f = function(c) {
          new_list <- s
          for (set_name in names(s$subset)) {
            new_list[["subset"]][[set_name]] <- ele_comb[[set_name]][c]
          }
          return(new_list)
        }
      )
    }
  })

  if (!all(purrr::map_lgl(multi_ele_shks, is.null))) {
    browser()
    flattened_shks <- purrr::list_flatten(purrr::compact(multi_ele_shks))
    multi_id <- unique(purrr::map_chr(flattened_shks, attr, "call_id"))
    shocks <- shocks[!map_chr(shocks, attr, "call_id") %in% multi_id]
    shocks <- c(shocks, flattened_shks)
  }

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

  #class(final_shocks) <- "shock"
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