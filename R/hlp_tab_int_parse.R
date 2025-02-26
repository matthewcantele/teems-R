#' @keywords internal
#' @noRd
.parse_tab_int <- function(expr,
                           ninterval) {
  expr <- gsub(
    pattern = "^\\(|\\)$",
    replacement = "",
    x = expr
  )
  terms <- strsplit(x = expr, split = "-|\\s*-\\s*")[[1]]

  .convert_p_term <- function(term) {
    content <- gsub(
      pattern = "P\\[|\\]",
      replacement = "",
      x = term
    )
    content <- gsub(
      pattern = "NINTERVAL",
      replacement = as.character(x = ninterval),
      x = content
    )
    eval(expr = parse(text = content))
  }

  if (grepl(
    pattern = "P\\[.*\\]\\s*-\\s*P\\[",
    x = expr
  )) {
    terms <- strsplit(
      x = expr,
      split = "\\s*-\\s*(?=P\\[)",
      perl = TRUE
    )[[1]]
    start <- .convert_p_term(term = terms[1])
    end <- .convert_p_term(term = terms[2])
    num_vec <- c(start:end)
  } else {
    num_vec <- .convert_p_term(term = expr)
  }
  return(num_vec)
}
