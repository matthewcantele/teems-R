#' @keywords internal
#' @noRd
.append_tablo <- function(tab,
                          coeff_extract,
                          sets) {

  # File
  # DATA # file containing all Base Data #;
  # (new) OutFile # output file #;
  sets[["name"]] <- toupper(x = sets[["name"]])
  set_writeout <- paste("File\n",
                        "(new)",
                        sets[["name"]],
                        "#",
                        sets[["name"]],
                        "output file #;\nWrite\n",
                        "(set)",
                        sets[["name"]],
                        "to file",
                        sets[["name"]],
                        "header",
                        paste0('"', sets[["name"]], '"'),
                        "longname",
                        paste0('"', trimws(x = gsub(pattern = "#", replacement = "", x = sets[["information"]])), '"', ";"))

  # coefficient writeout
  coeff_writeout <- paste(
    "File\n",
    "(new)",
    coeff_extract[["name"]],
    "#",
    coeff_extract[["name"]],
    "output file #;\nWrite\n",
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
    "\n#!< File and Write statements follow >!#\n",
    set_writeout,
    coeff_writeout),
    collapse = "\n"
  )

  return(tab)
}
