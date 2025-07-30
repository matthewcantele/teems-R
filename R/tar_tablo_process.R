#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @importFrom cli cli_h1 cli_dl cli_fmt
#' 
#' @keywords internal
#' @noRd
.process_tablo <- function(tab_file,
                           var_omit = NULL,
                           type = NULL,
                           call) {
  tab_comp <- .check_tab_file(
    tab_file = tab_file,
    call = call
  )

  # var omission
  if (!is.null(var_omit)) {
    for (var in unique(var_omit)) {
      tab_comp$state <- .omit_var(
        var_omit = var,
        statements = tab_comp$state
      )
    }
  }

  # extract prep
  extract <- tibble::tibble(
    type = purrr::map_chr(strsplit(tab_comp$state, split = " ", perl = TRUE), 1),
    remainder = sub("^\\S+\\s*", "", tab_comp$state)
  )

  if (is.null(type)) {
    var_extract <- .tablo_variables(
      tab_extract = extract,
      call = call
    )

    coeff_extract <- .tablo_coeff(
      tab_extract = extract,
      call = call
    )

    set_extract <- .tablo_sets(
      tab_extract = extract,
      call = call
    )

    math_extract <- .tablo_maths(
      tab_extract = extract,
      call = call
    )
    
    file_extract <- .tablo_files(
      tab_extract = extract,
      call = call
    )

    if (.o_verbose()) {
      n_var <- nrow(var_extract)
      n_eq <- nrow(subset(math_extract, math_extract$type %in% "Equation"))
      n_form <- nrow(subset(math_extract, math_extract$type %in% "Formula"))
      n_coeff <- nrow(coeff_extract)
      n_sets <- nrow(set_extract$sets)
      n_subsets <- nrow(x = set_extract$subsets)
      summary <- cli::cli_fmt({
        cli::cli_h1("Tablo file summary statistics:")
        cli::cli_dl(c(
          "Variables" = n_var,
          "Equations" = n_eq,
          "Coefficients" = n_coeff,
          "Formulas" = n_form,
          "Sets" = n_sets,
          "Subsets" = n_subsets
        ))
      })
    } else {
      summary <- NA
    }

    # no check for whether var_omit existed
    conden_tab <- paste0(tab_comp$state, ";")

    # add other statements eventually
    tab_comp <- list(
      conden_tab = conden_tab,
      orig_tab = tab_comp$orig,
      var_extract = var_extract,
      coeff_extract = coeff_extract,
      set_extract = set_extract,
      math_extract = math_extract,
      file_extract = file_extract,
      summary = summary
    )
    class(tab_comp$orig_tab) <- "tabfile"
  } else {
    fun_call <- switch(type,
      "variable" = ".tablo_variables",
      "coefficient" = ".tablo_coeff",
      "set" = ".tablo_sets"
    )
    tab_comp <- match.fun(fun_call)(extract, call)
  }

  return(tab_comp)
}