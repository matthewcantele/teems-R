#' @keywords internal
#' @noRd
.append_tablo <- function(tab,
                          coeff_extract,
                          sets) {
  # File
  # DATA # file containing all Base Data #;
  # (new) OutFile # output file #;
  sets$name <- toupper(sets$name)
  set_writeout <- paste(
    "File",
    "(new)",
    sets$name,
    "#",
    sets$name,
    "output file #;\nWrite",
    "(set)",
    sets$name,
    "to file",
    sets$name,
    "header",
    paste0('"', sets$name, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", sets$label)), '"', ";")
  )

  # coefficient writeout
  coeff_writeout <- paste(
    "File",
    "(new)",
    coeff_extract$coefficient,
    "#",
    coeff_extract$coefficient,
    "output file #;\nWrite",
    coeff_extract$coefficient,
    "to file",
    coeff_extract$coefficient,
    "header",
    paste0('"', coeff_extract$coefficient, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", coeff_extract$label)), '"', ";")
  )

  # here we append still due to potential for more write statements
  tab <- paste(
    c(
      tab,
      "\n#!< File and Write statements follow >!#\n",
      set_writeout,
      coeff_writeout
    ),
    collapse = "\n"
  )

  class(tab) <- "tabfile"
  return(tab)
}