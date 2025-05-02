#' @keywords internal
#' @noRd
.omit_var <- function(o,
                      tab) {
  var_declaration <- paste0("[Vv]ariable[^;]*\\b", o, "\\b[^;]*;")
  tab <- sub(
    pattern = var_declaration,
    replacement = "",
    x = tab,
    perl = TRUE
  )

  operator <- "\\s*[\\+\\-\\*\\/]\\s*"
  until_paren <- "\\([^\\)]*\\)"
  op_var <- paste0(operator, o, until_paren)
  tab <- gsub(
    pattern = op_var,
    replacement = "",
    x = tab,
    perl = TRUE
  )

  var_op <- paste0(o, until_paren, operator)
  tab <- gsub(
    pattern = var_op,
    replacement = "",
    x = tab,
    perl = TRUE
  )

  # this case should not occur
  var_wo_op <- paste0(o, until_paren)
  tab <- gsub(
    pattern = var_wo_op,
    replacement = "",
    x = tab,
    perl = TRUE
  )
  return(tab)
}