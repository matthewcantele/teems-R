#' @importFrom tibble tibble
#'
#' @keywords internal
#' @noRd
.retrieve_tab_comp <- function(tab_path,
                               type,
                               call) {
  tab <- readLines(con = tab_path)
  tab <- gsub(pattern = ";", replacement = "", x = tab)
  tab <- tab[1:which(is.element(el = tab, set = ""))[1] - 1]
  extract <- tibble::tibble(
    type = sapply(X = strsplit(x = tab, split = " ", perl = TRUE), "[[", 1),
    remainder = sub(pattern = "^\\S+\\s*", replacement = "", x = tab)
  )

  if (identical(x = type, y = "variable")) {
    comp_extract <- .tablo_variables(
      tab_extract = extract,
      call = call
    )
  } else if (is.element(el = type, set = c("coefficient", "inputdata"))) {
    comp_extract <- .tablo_coeff(
      tab_extract = extract,
      call = call
    )
  } else if (identical(x = type, y = "set")) {
    comp_extract <- NULL
  }
  return(comp_extract)
}