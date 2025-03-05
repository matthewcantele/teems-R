#' @importFrom targets tar_read
#' @importFrom qs2 qs_read
#' 
#' @keywords internal
#' @noRd
.retrieve_output <- function(type,
                             paths,
                             sets,
                             int,
                             call) {
  #UseMethod here eventually
  if (identical(x = type, y = "variable")) {
    var_union <- .unite_csvs(target = "var_csvs",
                             paths = paths[["var"]],
                             call = call)
    tablo_var <- targets::tar_read(name = tablo_var,
                                   store = file.path(paths[["model"]],
                                                     "store"))
    output <- .parse_var(
      paths = paths[["var"]],
      var_extract = tablo_var,
      vars = var_union,
      sets = sets[["postmodel"]],
      chron_yrs = int[["CYRS"]],
      call = call
    )
  } else if (is.element(el = type, set = c("coefficient", "basedata"))) {
    coeff_extract <- targets::tar_read(name = coeff_extract,
                                       store = file.path(paths[["model"]],
                                                         "store"))
    output <- .parse_coeff(
      paths = paths[["coeff"]],
      coeff_extract = coeff_extract,
      sets = sets[["postmodel"]],
      chron_yrs = int[["CYRS"]],
      call = call
    )
    
    if (identical(x = type, y = "basedata")) {
      pre_coeff <- targets::tar_read(name = final.base_tib,
                                     store = file.path(paths[["model"]],
                                                       "store"))
      reference_year <- qs2::qs_read(file = paths[["metadata"]])[["reference_year"]]
      output <- .merge_data(
        pre_coeff = pre_coeff[["dt"]],
        post_coeff = output,
        sets = sets[["premodel"]],
        coeff_extract = coeff_extract,
        reference_year = reference_year,
        intertemporal = int[["intertemporal"]]
      )
    }
  } else if (identical(x = type, y = "set")) {
    output <- .parse_set(paths = paths[["set"]],
                         sets = sets[["premodel"]],
                         call = call)
  }
  return(output)
}