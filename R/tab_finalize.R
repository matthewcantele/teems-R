#' @keywords internal
#' @noRd
.finalize_tab <- function(model) {
  set_extract <- subset(model, type == "Set")
  coeff_extract <- subset(model, type == "Coefficient")

  set_extract$name <- toupper(set_extract$name)
  set_writeout <- paste(
    "File",
    "(new)",
    set_extract$name,
    "#",
    set_extract$name,
    "output file #;\nWrite",
    "(set)",
    set_extract$name,
    "to file",
    set_extract$name,
    "header",
    paste0('"', set_extract$name, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", set_extract$label)), '"', ";")
  )

  coeff_writeout <- paste(
    "File",
    "(new)",
    coeff_extract$name,
    "#",
    coeff_extract$name,
    "output file #;\nWrite",
    coeff_extract$name,
    "to file",
    coeff_extract$name,
    "header",
    paste0('"', coeff_extract$name, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", coeff_extract$label)), '"', ";")
  )

  tab <- paste(
    c(
      model$tab,
      "\n#!< File and Write statements follow >!#\n",
      set_writeout,
      coeff_writeout
    ),
    collapse = "\n"
  )

  if (inherits(attr(model, "tab_file"), "internal")) {
    attr(tab, "file") <- paste0(attr(model, "tab_file"), ".tab")
  } else {
    attr(tab, "file") <- attr(input, "tab_file")
  }

  class(tab) <- c("tab", class(tab))
  return(tab)
}