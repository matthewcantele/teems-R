#' @importFrom cli cli_h1 cli_li
#' 
#' @keywords internal
#' @noRd
.check_closure_file <- function(closure_file,
                                tab_file,
                                var_omit,
                                quiet,
                                call) {
  if (is.null(x = closure_file)) {
    closure_file <- .infer_closure(
      tab_file = tab_file,
      quiet = quiet,
      call = call
    )
  }

  closure <- tail(head(readLines(con = closure_file), -3), -1)
  mod_cls <- setdiff(x = closure, y = var_omit)
  # doesn't take swaps into consideration so mute
  # if (!quiet) {
  #   cli::cli_h1("Current exogenous variables")
  #   cli::cli_li(items = mod_cls)
  # }
  return(closure_file)
}