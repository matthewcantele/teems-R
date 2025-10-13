#' @importFrom purrr map
#' 
#' @keywords internal
#' @noRd
.validate_closure <- function(closure,
                              swap_in,
                              swap_out,
                              shock,
                              shock_file,
                              sets,
                              var_extract,
                              call) {
  closure_file <- attr(closure, "file")
  closure <- .classify_cls(closure = closure,
                           sets = sets)

  closure <- lapply(closure,
                    .exp_cls_entry,
                    var_extract = var_extract,
                    sets = sets$ele,
                    call = call)

  var_names <- purrr::map_chr(closure, attr, "var_name")
  multi_entry <- closure[duplicated(var_names) | duplicated(var_names, fromLast = TRUE)]
  for (e in unique(purrr::map_chr(multi_entry, attr, "var_name"))) {
    over_check <- multi_entry[purrr::map_chr(multi_entry, attr, "var_name") == e]
    over_check <- data.table::rbindlist(lapply(over_check, attr, "ele"))
    if (any(duplicated(over_check))) {
      overlap <- over_check[duplicated(over_check)]
      n_overlap <- nrow(overlap)
      overlap <- capture.output(print(overlap))[-c(1, 2)]
      .cli_action(swap_err$overlap_ele,
                  action = "abort",
                  call = call)
    }
  }

  attr(closure, "file") <- closure_file
  return(closure)
}