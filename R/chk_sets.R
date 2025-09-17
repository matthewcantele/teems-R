#' @importFrom purrr map2
#' 
#' @keywords internal
#' @noRd
.check_sets <- function(bin_csv_paths,
                        model_dir,
                        set_path,
                        call) {
  sets <- .unite_csvs(
    target = "set_csvs",
    paths = bin_csv_paths
  )
  sets <- .match_set_ele(
    sets_out = sets,
    paths = bin_csv_paths
  )
  if (.o_post_set_check()) {
    tab_sets <- lapply(set_path, function(s) {
      tail(head(readLines(s), -1), -1)
    })
    names(tab_sets) <- gsub("\\.csv", "", basename(set_path))
    .check_set_consistency(
      bin_sets = sets,
      tab_sets = tab_sets,
      call = call
    )
  }

  if (any(sets$intertemp == 1L)) {
    sets$ele <- purrr::map2(
      sets$intertemp,
      sets$ele,
      function(int, e) {
        if (int %=% 1L) {
          e <- as.numeric(e)
          attr(e, "intertemporal") <- TRUE
        }
        return(e)
      }
    )
  }

  names(sets$ele) <- sets$setname
  sets <- subset(sets, select = ele, drop = 1)
  return(sets)
}