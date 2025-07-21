#' @keywords internal
#' @noRd
.parse_tab_int <- function(expr,
                           n_timestep,
                           n_timestep_coeff) {

  terms <- strsplit(x = expr, split = "-|\\s*-\\s*")[[1]]
  if (grepl(
    pattern = "P\\[.*\\]\\s*-\\s*P\\[",
    x = expr
  )) {
    terms <- strsplit(
      x = expr,
      split = "\\s*-\\s*(?=P\\[)",
      perl = TRUE
    )[[1]]
    start <- .convert_p_term(term = terms[1],
                             n_timestep = n_timestep,
                             n_timestep_coeff = n_timestep_coeff)
    end <- .convert_p_term(term = terms[2],
                           n_timestep = n_timestep,
                           n_timestep_coeff = n_timestep_coeff)
    num_vec <- c(start:end)
  } else {
    num_vec <- .convert_p_term(term = expr,
                               n_timestep = n_timestep,
                               n_timestep_coeff = n_timestep_coeff)
  }

  return(num_vec)
}

.convert_p_term <- function(term,
                            n_timestep,
                            n_timestep_coeff) {

  content <- gsub(
    pattern = "P\\[|\\]",
    replacement = "",
    x = term
  )
  content <- gsub(
    pattern = n_timestep_coeff,
    replacement = as.character(x = n_timestep),
    x = content
  )
  eval(expr = parse(text = content))
}
