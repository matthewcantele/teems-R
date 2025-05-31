#' @importFrom data.table fread setnames
#' @importFrom purrr map2
#' 
#' @keywords internal
#' @noRd
.match_set_ele <- function(sets_out,
                           paths) {
  # load elements
  set_ele_file <- list.files(path = paths, pattern = "setele", full.names = TRUE)
  set_ele <- data.table::fread(input = set_ele_file, header = FALSE, skip = 1)
  data.table::setnames(x = set_ele, new = c("r_idx", "mapped_ele"))

  # match set elements to sets
  sets_out[["mapped_ele"]] <- purrr::map2(
    .x = sets_out[["begadd"]],
    .y = sets_out[["size"]],
    .f = function(row_id, l) {
      start <- row_id + 1 # indexing starts at 0
      stop <- row_id + l
      set_ele[seq(start, stop), mapped_ele]
    }
  )

  return(sets_out)
}
