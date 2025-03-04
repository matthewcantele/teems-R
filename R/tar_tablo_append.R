#' .append_tablo function
#'
#' This function appends write information to a Tablo file by writing out
#' coefficients and headers. In the case of recursive runs, it appends the
#' appropriate dedicated data and parameter write statements to the tablo.
#'
#' @param tab A list representing the tablo to be augmented.
#'
#' @importFrom purrr map2
#' @return The augmented tablo.
#' @keywords internal
#' @noRd
.append_tablo <- function(tab,
                          coeff_extract,
                          sets) {

  sets[["name"]] <- toupper(x = sets[["name"]])
  set_writeout <- paste("(set)",
                        sets[["name"]],
                        "to file",
                        sets[["name"]],
                        "header",
                        paste0('"', sets[["name"]], '"'),
                        "longname",
                        paste0('"', trimws(x = gsub(pattern = "#", replacement = "", x = sets[["information"]])), '"', ";"))

  # coefficient writeout
  coeff_writeout <- paste(
    coeff_extract[["name"]],
    "to file",
    coeff_extract[["name"]],
    "header",
    paste0('"', coeff_extract[["name"]], '"'),
    "longname",
    paste0('"', trimws(x = gsub(pattern = "#", replacement = "", x = coeff_extract[["information"]])), '"', ";")
  )
  
  

  # here we append still due to potential for more write statements
  tab <- paste(c(tab,
    "\n#!< Write statements follow >!#\n",
    "Write",
    set_writeout,
    coeff_writeout),
    collapse = "\n"
  )

  return(tab)
}
