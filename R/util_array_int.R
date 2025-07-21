#' @importFrom purrr pluck
#'
#' @keywords internal
#' @noRd
.int_array <- function(input,
                       coeff_extract,
                       int_sets) {

  if (input$header %in% coeff_extract$header) {
    data_dnames <- names(dimnames(input$data))
    tab_dnames <- purrr::pluck(coeff_extract, "ls_upper_idx", input$header)
    if (!data_dnames %=% tab_dnames && any(int_sets$name %in% tab_dnames)) {
      int_set_name <- intersect(int_sets$name, tab_dnames)
      int_set <- subset(int_sets, name %in% tab_dnames, elements)[[1]]
      l_int_set <- length(int_set[[1]])
      input$data <- array(
        rep(input$data, l_int_set),
        dim = c(dim(input$data), l_int_set),
        dimnames = c(dimnames(input$data), int_set)
      )
    }
  } 
  return(input)
}