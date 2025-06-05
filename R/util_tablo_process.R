#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @importFrom cli cli_h1 cli_dl
#' 
#' @keywords internal
#' @noRd
.process_tablo <- function(tab_file,
                           var_omit = NULL,
                           type = NULL,
                           call,
                           quiet = TRUE) {
  tab_comp <- .check_tab_file(tab_file = tab_file,
                              call = call,
                              quiet = quiet)
  
  # var omission
  if (!is.null(x = var_omit)) {
    for (var in unique(x = var_omit)) {
      tab_comp[["state"]] <- .omit_var(var_omit = var,
                              statements = tab_comp[["state"]])
    }
  }

  # extract prep
  extract <- tibble::tibble(
    type = purrr::map_chr(.x = strsplit(x = tab_comp[["state"]], split = " ", perl = TRUE),
                          .f = 1),
    remainder = sub(pattern = "^\\S+\\s*", replacement = "", x = tab_comp[["state"]])
  )
  
  if (is.null(x = type)) {
  var_extract <- .tablo_variables(tab_extract = extract,
                                  call = call)
  
  coeff_extract <- .tablo_coeff(tab_extract = extract,
                                call = call)
  
  set_extract <- .tablo_sets(tab_extract = extract,
                             call = call)

  math_extract <- .tablo_maths(tab_extract = extract,
                               call = call)
  if (!quiet) {
    n_var <- nrow(x = var_extract)
    n_eq <- nrow(x = subset(x = math_extract,
                            subset = {is.element(el = math_extract[["type"]],
                                                 set = "Equation")}))
    n_form <- nrow(x = subset(x = math_extract,
                              subset = {is.element(el = math_extract[["type"]],
                                                   set = "Formula")}))
    n_coeff <- nrow(x = coeff_extract)
    n_sets <- nrow(x = set_extract[["sets"]])
    n_subsets <- nrow(x = set_extract[["subsets"]])
    cli::cli_h1(text = "Tablo file summary statistics:")
    cli::cli_dl(items = c("Variables" = n_var,
                          "Equations" = n_eq,
                          "Coefficients" = n_coeff,
                          "Formulas" = n_form,
                          "Sets" = n_sets,
                          "Subsets" = n_subsets))
  }
  
  # no check for whether var_omit existed
  conden_tab <- paste0(tab_comp[["state"]], ";")
  
  # add other statements eventually
  tab_comp <- list(conden_tab = conden_tab,
                   orig_tab = tab_comp[["orig"]],
                   var_extract = var_extract,
                   coeff_extract = coeff_extract,
                   set_extract = set_extract,
                   math_extract = math_extract)
  } else {
    fun_call <- switch(EXPR = type,
                       "variable" = ".tablo_variables",
                       "coefficient" = ".tablo_coeff",
                       "set" = ".tablo_sets")
    tab_comp <- match.fun(FUN = fun_call)(extract, call)
  }
  return(tab_comp)
}