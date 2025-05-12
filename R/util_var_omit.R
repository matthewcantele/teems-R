#' @keywords internal
#' @noRd
.omit_var <- function(var_omit,
                      statements,
                      extract) {

  # remove var declaration
  dec_pattern <- paste0("^[Vv]ariable.*(?<![[:alnum:]_])", var_omit, "\\(")
  statements <- statements[!grepl(pattern = dec_pattern, x = statements, perl = TRUE)]
  
  operator <- "\\s*[\\+\\-\\*\\/]\\s*"
  until_paren <- "\\([^\\)]*\\)"
  
  op_var <- paste0(operator, "(?<![[:alnum:]_])", var_omit, until_paren)
  var_op <- paste0("(?<![[:alnum:]_])", var_omit, until_paren, operator)
  var_wo_op <- paste0("(?<![[:alnum:]_])", var_omit, until_paren)

  statements <- gsub(pattern = op_var, x = statements, replacement = "", perl = TRUE)
  statements <- gsub(pattern = var_op, x = statements, replacement = "", perl = TRUE)
  statements <- gsub(pattern = var_wo_op, x = statements, replacement = "", perl = TRUE)

  return(statements)
}