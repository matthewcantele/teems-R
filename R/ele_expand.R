#' @importFrom purrr list_flatten map_lgl map
#' 
#' @noRd
#' @keywords internal
.expand_ele <- function(input,
                        nested = FALSE) {

  if (nested && is.list(input)) {
    fl_input <- purrr::list_flatten(input)
  } else {
    fl_input <- input
  }
  
  if (any(purrr::map_lgl(fl_input, inherits, "multi"))) {
    multi_ele <- lapply(fl_input, function(m) {
      if (inherits(m, "multi")) {
        ele_comb <- expand.grid(m$subset, stringsAsFactors = FALSE)
        purrr::map(
          .x = seq_len(nrow(ele_comb)),
          .f = function(c) {
            new_list <- m
            for (set_name in names(m$subset)) {
              new_list[["subset"]][[set_name]] <- ele_comb[[set_name]][c]
            }
            class(new_list) <- setdiff(class(new_list), "multi")
            return(new_list)
          }
        )
      } else {
        m
      }
    })
    
    input <- purrr::list_flatten(multi_ele)
  }
  
  return(input)
}
