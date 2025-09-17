#' @importFrom data.table fread setnames
#' @importFrom purrr map2
#' 
#' @keywords internal
#' @noRd
.match_set_ele <- function(sets_out,
                           paths) {

  set_ele_file <- list.files(paths,
    "setele",
    full.names = TRUE
  )
  set_ele <- data.table::fread(set_ele_file,
    header = FALSE,
    skip = 1
  )
  data.table::setnames(set_ele, new = c("r_idx", "mapped_ele"))

  sets_out$ele <- purrr::map2(
    .x = sets_out[["begadd"]],
    .y = sets_out[["size"]],
    .f = function(row_id, l) {
      start <- row_id + 1
      stop <- row_id + l
      set_ele[seq(start, stop), mapped_ele]
    }
  )
  
  sets_out$setname <- toupper(sets_out$setname)
  names(sets_out$ele) <- sets_out$setname
  return(sets_out)
}