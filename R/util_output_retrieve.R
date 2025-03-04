.retrieve_output <- function(type,
                             paths,
                             call) {
  browser()
  if (identical(x = type, y = "variable")) {
    var_union <- .unite_csvs(target = "var_csvs",
                             paths = var_paths,
                             call = call)
    targets::tar_load(name = tablo_var, store = file.path(model_dir, "store"))
    output <- .parse_var(
      paths = var_paths,
      var_extract = tablo_var,
      vars = var_union,
      sets = set_elements,
      chron_yrs = CYRS
    )
  } else if (identical(x = type, y = "coefficient")) {
    coeff_extract <- targets::tar_read(name = coeff_extract,
                                       store = file.path(model_dir, "store"))
    output <- .parse_coeff(
      paths = coeff_paths,
      coeff_extract = coeff_extract,
      sets = set_elements,
      chron_yrs = CYRS
    )
  } else if (identical(x = type, y = "set")) {
    output <- .parse_set(
      paths = coeff_paths,
      coeff_extract = coeff_extract,
      sets = set_elements,
      chron_yrs = CYRS
    )
  }
}