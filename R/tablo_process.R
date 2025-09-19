#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_chr
#' @importFrom cli cli_h1 cli_dl cli_fmt
#' 
#' @keywords internal
#' @noRd
.process_tablo <- function(tab_file,
                           var_omit = NULL,
                           type = NULL,
                           call) {

  tab <- .check_tab_file(
    tab_file = tab_file,
    call = call
  )

  if (!is.null(var_omit)) {
    for (var in unique(var_omit)) {
      tab <- .omit_var(
        var_omit = var,
        statements = tab
      )
    }
  }

  extract <- tibble::tibble(
    type = purrr::map_chr(strsplit(tab, split = " ", perl = TRUE), 1),
    remainder = sub("^\\S+\\s*", "", tab)
  )

  extract$row_id <- seq(1, nrow(extract))
  var_extract <- .parse_tab_obj(
    extract = extract,
    obj_type = "variable",
    call = call
  )

  coeff_extract <- .parse_tab_obj(
    extract = extract,
    obj_type = "coefficient",
    call = call
  )

  set_extract <- .parse_tab_sets(
    tab_extract = extract,
    call = call
  )

  if (.o_verbose()) {
    math_extract <- .tablo_maths(
      tab_extract = extract,
      call = call
    )

    n_var <- nrow(var_extract)
    n_eq <- nrow(subset(math_extract, math_extract$type %in% "Equation"))
    n_form <- nrow(subset(math_extract, math_extract$type %in% "Formula"))
    n_coeff <- nrow(coeff_extract)
    n_sets <- nrow(set_extract)

    cli::cli_h1("Tablo file summary statistics:")
    cli::cli_dl(c(
      "Variables" = n_var,
      "Equations" = n_eq,
      "Coefficients" = n_coeff,
      "Formulas" = n_form,
      "Sets" = n_sets
    ))
  }

  tab <- paste0(tab, ";")
  tab <- tibble::tibble(
    tab = tab,
    row_id = seq(1, length(tab))
  )

  tab_parsed <- rbind(var_extract, coeff_extract, set_extract)
  tab <- tibble::as_tibble(merge(tab_parsed, tab, by = "row_id", all = TRUE))
  tab <- tab[order(tab$row_id), ]

  tab$type <- ifelse(is.na(tab$type),
    extract$type,
    tab$type
  )
  tab$row_id <- NULL
  
  tab <- structure(tab,
                   tab_file = tab_file,
                   var_omit = var_omit,
                   class = c("model", class(tab)))

  return(tab)
}