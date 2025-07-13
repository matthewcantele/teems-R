#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.check_closure_file <- function(closure_file,
                                tab_file,
                                var_omit,
                                var_extract,
                                quiet,
                                call) {
  if (is.null(closure_file)) {
    closure_file <- .infer_closure(
      tab_file = tab_file,
      quiet = quiet,
      call = call
    )
  }

  closure <- tail(head(readLines(con = closure_file), -3), -1)
  mod_cls <- setdiff(closure, var_omit)
  cls_var <- purrr::map_chr(strsplit(mod_cls, split = "\\("), 1)
  if (!all(tolower(cls_var) %in% tolower(var_extract$name))) {
    var_discrepancy <- setdiff(tolower(cls_var), tolower(var_extract$name))
    l_var <- length(var_discrepancy)
    .cli_action(cls_err$no_var,
      action = "abort",
      call = call
    )
  }

  return(closure_file)
}