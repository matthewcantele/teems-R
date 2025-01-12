.get_dynamics <- function(tab_file,
                          user_tab) {
  if (user_tab) {
    if (any(grepl(pattern = "(intertemporal)", x = readLines(con = tab_file))))
    {
      temporal_dynamics <- "intertemporal"
    } else {
      temporal_dynamics <- "static"
    }
  } else {
    internal_tab <- subset(x = internal_tab, subset = {
      is.element(el = names(x = internal_tab), set = tab_file)
    })
    if (any(grepl(pattern = "(intertemporal)", x = internal_tab)))
    {
      temporal_dynamics <- "intertemporal"
    } else {
      temporal_dynamics <- "static"
    }
  }
  return(temporal_dynamics)
}
