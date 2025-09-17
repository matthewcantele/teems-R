#' @keywords internal
#' @noRd
.omit_var <- function(var_omit,
                      statements,
                      extract) {
  dec_pattern <- paste0("^[Vv]ariable.*(?<![[:alnum:]_])", var_omit, "\\(")
  statements <- statements[!grepl(dec_pattern, statements, perl = TRUE)]
  until_paren <- "\\([^\\)]*\\)"
  op_var <- paste0("(?<![[:alnum:]_])", var_omit, until_paren)
  statements <- gsub(op_var,
    "0",
    statements,
    perl = TRUE
  )
  return(statements)
}