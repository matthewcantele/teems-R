#' @importFrom cli col_blue cli_inform
#'
#' @keywords internal
#' @noRd
.set_ele_inform <- function(set_ele,
                            set_name,
                            model_set) {
  element_text <- cli::col_blue(sort(x = paste(set_ele[[1]])))
  cli::cli_inform(c(
    "i" = "Set elements for: {set_name}s ({model_set}) determined as:",
    ">" = "{element_text}"
  ))
}
