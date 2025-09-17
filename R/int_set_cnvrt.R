#' @importFrom data.table data.table 
#'
#' @keywords internal
#' @noRd
.convert_int_sets <- function(expr,
                              n_timestep,
                              n_timestep_header) {

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
                             n_timestep_header = n_timestep_header)
    end <- .convert_p_term(term = terms[2],
                           n_timestep = n_timestep,
                           n_timestep_header = n_timestep_header)
    num_vec <- c(start:end)
  } else {
    num_vec <- .convert_p_term(term = expr,
                               n_timestep = n_timestep,
                               n_timestep_header = n_timestep_header)
  }
  
  mapping <- data.table::data.table(origin = num_vec,
                                    mapping = num_vec,
                                    key = c("origin", "mapping"))
  return(mapping)
}

.convert_p_term <- function(term,
                            n_timestep,
                            n_timestep_header) {

  content <- gsub(
    pattern = "P\\[|\\]",
    replacement = "",
    x = term
  )
  content <- gsub(
    pattern = n_timestep_header,
    replacement = as.character(x = n_timestep),
    x = content
  )
  eval(expr = parse(text = content))
}
