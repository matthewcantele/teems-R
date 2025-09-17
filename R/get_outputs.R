#' @importFrom tibble add_column
#' 
#' @keywords internal
#' @noRd
.get_outputs <- function(cmf_path) {
  var <- ems_compose(
    cmf_path = cmf_path,
    which = "variable"
  )
  var <- tibble::add_column(var, type = "variable", .after = "label")
  coeff <- ems_compose(
    cmf_path = cmf_path,
    which = "coefficient"
  )
  coeff <- tibble::add_column(coeff, type = "coefficient", .after = "label")
  output <- rbind(var, coeff)
  return(output)
}