#' Match Set Elements to Sets
#'
#' This function matches set elements to sets based on beginning addresses and
#' sizes specified in `sets_out`. It loads set elements from files matching a
#' pattern in the specified paths, then assigns these elements to the
#' corresponding sets.
#'
#' @param sets_out A list containing the sets' beginning addresses (`begadd`)
#'   and sizes (`size`).
#' @param paths The directory paths where set element files are located.
#'
#' @importFrom data.table fread setnames
#' @importFrom purrr map2
#' @return The `sets_out` list with an additional `elements` list containing the
#'   matched set elements.
#' @keywords internal
#' @noRd
.match_set_ele <- function(sets_out,
                        paths) {
  # load elements
  set_ele_file <- list.files(path = paths, pattern = "setele", full.names = TRUE)
  set_ele <- data.table::fread(input = set_ele_file, header = FALSE, skip = 1)
  data.table::setnames(x = set_ele, new = c("r_idx", "elements"))

  # match set elements to sets
  sets_out[["elements"]] <- purrr::map2(
    .x = sets_out[["begadd"]],
    .y = sets_out[["size"]],
    .f = function(row_id, l) {
      start <- row_id + 1 # indexing starts at 0
      stop <- row_id + l
      set_ele[seq(start, stop), elements]
    }
  )

  return(sets_out)
}
