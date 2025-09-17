#' @importFrom purrr pluck map_lgl
#' 
#' @keywords internal
#' @noRd
.retrieve_output <- function(type,
                             comp_extract,
                             name,
                             paths,
                             sets,
                             time_steps,
                             call) {

  if (type %=% "variable") {
    var_union <- .unite_csvs(
      target = "var_csvs",
      paths = paths$bin_csv,
      call = call
    )

    output <- .parse_var(
      paths = paths$bin_csv,
      var_extract = comp_extract,
      vars = var_union,
      sets = sets,
      time_steps = time_steps,
      call = call
    )
  } else if (type %=% "coefficient") {
    output <- .parse_coeff(
      paths = paths$coeff,
      coeff_extract = comp_extract,
      sets = sets,
      time_steps = time_steps,
      call = call
    )
  }

  if (!is.null(name)) {
    if (length(name) > 1) {
      output <- output[output$name %in% name, ]
    } else {
      output <- purrr::pluck(output, "dat", name)
    }
  }
  
  return(output)
}
