#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.check_closure_file <- function(closure_file,
                                tab_file,
                                var_omit,
                                var_extract,
                                call) {

  if (is.null(closure_file)) {
    if (inherits(tab_file, "internal")) {
    closure <- internal_cls[[tab_file]]
    closure_file <- paste0(tab_file, ".cls")
    } else {
      .cli_action(cls_err$no_cls,
                  action = "abort",
                  call = call)
    }
  } else {
    closure <- readLines(closure_file)
    closure_file <- basename(closure_file)
  }

  closure <- tail(head(closure, -3), -1)
  closure <- closure[!closure %in% var_omit]
  cls_var <- purrr::map_chr(strsplit(closure, "\\("), 1)
  if (!all(tolower(cls_var) %in% tolower(var_extract$name))) {
    var_discrepancy <- setdiff(tolower(cls_var), tolower(var_extract$name))
    l_var <- length(var_discrepancy)
    .cli_action(cls_err$no_var,
      action = "abort",
      call = call
    )
  }
  
  attr(closure, "file") <- closure_file
  return(closure)
}