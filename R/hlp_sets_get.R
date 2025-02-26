#' @keywords internal
#' @noRd
.get_sets <- function(var,
                      var_extract,
                      type = c("upper", "lower", "mixed"),
                      listed = TRUE) {

  if (listed) {
    set_col <-  paste0(c("ls", type, "idx"), collapse = "_")
  } else {
    set_col <- paste0(c(type, "idx"), collapse = "_")
  }

  set_idx <- unlist(x = subset(x = var_extract,
                               subset = {is.element(el = tolower(x = name),
                                                    set = tolower(x = var))},
                               select = set_col),
                    use.names = FALSE)

  return(set_idx)
}
