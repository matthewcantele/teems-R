#' @importFrom purrr pluck
#' @importFrom data.table fread setcolorder fwrite
#' 
#' @noRd
#' @keywords internal
.check_cst_scen <- function(shock,
                            var_extract,
                            int_sets,
                            call) {

  ls_mixed <- purrr::pluck(var_extract, "ls_mixed_idx", shock$var)

  if (!is.null(int_sets)) {
    if (any(grepl(pattern = "Year", shock$set))) {
      int_sets <- paste0(int_sets, "t")
      if (any(int_sets %in% shock$set)) {
        supplied_int_set <- intersect(shock$set, int_sets)
        .cli_action(
          shk_err$extra_col,
          action = "abort",
          url = NULL,
          hyperlink = NULL,
          call = call
        )
      }
      set_check <- c(setdiff(ls_mixed, int_sets), "Year")
    } else {
      set_check <- ls_mixed
    }
    
  } else {
    set_check <- ls_mixed
  }

  if (!all(shock$set %in% set_check)) {
    errant_set <- setdiff(shock$set, set_check)
    l_errant_set <- length(errant_set)
    var_name <- shock$var
    .cli_action(
      shk_err$invalid_set,
      action = c("abort", "inform", "inform", "inform"),
      url = NULL,
      hyperlink = NULL,
      call = call
    )
  }

  if (!set_check %=% shock$set) {
    data.table::setcolorder(shock$input, c(set_check, "Value"))
    shock$set <- set_check
  }

  shock$ls_mixed <- ls_mixed
  shock$ls_upper <- purrr::pluck(var_extract, "ls_upper_idx", shock$var)
  return(shock)
}