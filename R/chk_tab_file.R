#' @keywords internal
#' @noRd
.check_tab_file <- function(tab_file,
                            model_version,
                            call,
                            quiet) {
  tab <- readChar(con = tab_file,
                  nchars = file.info(tab_file)[["size"]])
  
  statements <- .check_statements(tab = tab,
                                  ok_state = c("File", "Coefficient", "Read", "Update", "Set", "Subset",
                                               "Formula", "Assertion", "Variable", "Equation", "Write",
                                               "Zerodivide"),
                                  call = call)
  
  if (!quiet) {
    n_state <- length(x = statements)
    .cli_action(msg = "{.val {tab_file}} contains {.val {n_state}} statements.",
                action = "inform")
  }
  
  if (!is.null(x = model_version)) {
    choices <- c("v6.2", "v7.0")
    if (!is.element(el = model_version, set = choices)) {
      .cli_action(msg = "Invalid {.arg model_version}: {.val {model_version}}. 
                  Must be {.or {.val {choices}}}.",
                  action = "abort",
                  call = call)
    }
  } else {
    model_version <- .get_model_version(tab = tab,
                                        tab_file = tab_file,
                                        call = call,
                                        quiet = quiet)
  }
  
  tab_comps <- list(orig = tab,
                    state = statements,
                    model_version = model_version)
  return(tab_comps)
}