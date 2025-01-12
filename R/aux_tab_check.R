.tab_check <- function(tab_file) {
  if (grepl(pattern = "\\.tab", x = tab_file)) {
    tab_file <- path.expand(path = tab_file)
    if (!file.exists(tab_file)) {
      stop(paste("Filepath for", dQuote(x = "tab_file"), "is not found!"))
    } else {
      user_tab <- TRUE
    }
  } else if (!is.element(el = tab_file, set = names(x = internal_tab))) {
    stop(
      paste(
        "The chosen internal Tablo file:",
        tab_file,
        "is not supported.",
        "Currently supported Tablo files include:",
        paste(names(x = internal_tab), collapse = ", "),
        'Otherwise an unsupported Tablo file by directly inputting a Tablo path, e.g., "~/path2/tablo.tab"'
      )
    )
  } else {
    user_tab <- FALSE
  }
  return(user_tab)
}
