#' @keywords internal
#' @noRd
.check_tab_file <- function(tab_file,
                            call) {
  if (inherits(tab_file, "internal")) {
    tab <- internal_tab[[tab_file]]
  } else {
    tab <- readChar(
      tab_file,
      file.info(tab_file)[["size"]]
    )
  }

  statements <- .check_statements(
    tab = tab,
    ok_state = c(
      "File", "Coefficient", "Read", "Update", "Set", "Subset",
      "Formula", "Assertion", "Variable", "Equation", "Write",
      "Zerodivide"
    ),
    call = call
  )

  return(statements)
}
