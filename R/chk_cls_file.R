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
  if (is.null(x = closure_file)) {
    closure_file <- .infer_closure(
      tab_file = tab_file,
      quiet = quiet,
      call = call
    )
  }

  closure <- tail(x = head(x = readLines(con = closure_file), -3), -1)
  mod_cls <- setdiff(x = closure, y = var_omit)
  cls_var <- purrr::map_chr(.x = strsplit(x = mod_cls, split = "\\("), .f = 1)
  if (!all(is.element(el = tolower(x = cls_var), set = tolower(x = var_extract[["name"]])))) {
    var_discrepancy <- setdiff(x = tolower(x = cls_var), y = tolower(x = var_extract[["name"]]))
    l_var <- length(x = var_discrepancy)
    .cli_action(
      msg = "Found {l_var} variable{?s} from the closure file not
                present in the Tablo file: {.val {var_discrepancy}}.",
      action = "abort",
      call = call
    )
  }

  return(closure_file)
}