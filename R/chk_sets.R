#' @importFrom targets tar_read
#' 
#' @keywords internal
#' @noRd
.check_sets <- function(var_paths,
                        model_dir,
                        call) {
  set_union <- .unite_csvs(target = "set_csvs", paths = var_paths)
  set_ele <- .match_set_ele(sets_out = set_union, paths = var_paths)
  sets <- targets::tar_read(name = final.set_tib,
                            store = file.path(model_dir, "store"))
  sets[["name"]] <- toupper(x = sets[["name"]])
  .check_set_consistency(premodel = sets,
                         postmodel = set_ele,
                         call = call)
  sets <- list(premodel = sets,
               postmodel = set_ele)
  return(sets)
}
