#' @keywords internal
#' @noRd
.check_tab_file <- function(tab_file,
                            #model_version,
                            call) {
  tab <- readChar(tab_file,
                  file.info(tab_file)[["size"]])
  
  statements <- .check_statements(tab = tab,
                                  ok_state = c("File", "Coefficient", "Read", "Update", "Set", "Subset",
                                               "Formula", "Assertion", "Variable", "Equation", "Write",
                                               "Zerodivide"),
                                  call = call)

  tab_comps <- list(orig = tab,
                    state = statements)
                    #model_version = model_version)
  return(tab_comps)
}